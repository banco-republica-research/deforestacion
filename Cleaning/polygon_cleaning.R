##############################################################################################
##############################################################################################
###                                                                                        ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                                                                        ###
##############################################################################################
##############################################################################################

library(raster)
library(rgdal)
library(magrittr)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(lattice)
library(plyr)
library(grid)
library(FNN)
library(dplyr)
library(broom)
library(stringr)

setwd("/Volumes/LaCie/Deforestacion/Hansen")
res <- brick("loss_year_brick_1km.tif")

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
natural_parks <- readOGR(dsn = "WDPA_June2016_COL-shapefile", layer = "WDPA_June2016_COL-shapefile-polygons")
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Remove NP that are out of continental land and parks after 2012
natural_parks <- list(natural_parks, natural_parks_proj) %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park",
                           "Corales De Profundidad",
                           "Los Corales Del Rosario Y De San Bernardo",
                           "Gorgona",
                           "Acandi Playon Y Playona",
                           "Uramba Bahia Malaga")) & !x@data$STATUS_YR > 2012 & !x@data$GIS_AREA < 1 , ]
      
    
  }) %>%
  #Remove sections of park outside continental Colombia
  mapply(function(x, y){
    raster::intersect(y, x)
    }, x = . , y = colombia_municipios)

setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
writeOGR(obj = natural_parks[[2]], dsn = "WDPA_Modificado" , layer = "WDPA_clean", driver = "ESRI Shapefile", overwrite_layer = TRUE)

#Buffers to asses "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000)
buffers_natural_parks <- spTransform(buffers_natural_parks_proj, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Create a list of individual polygons per natural park
natural_parks[[1]]@data$ID <- as.factor(row.names(natural_parks[[1]]@data))
list_polygons <- list()
total <- length(natural_parks[[1]]@data$ID)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in natural_parks[[1]]@data$ID){
  list_polygons[[i]] <- natural_parks[[1]][natural_parks[[1]]@data$ID == i, ]
  setTxtProgressBar(pb, i)
}
close(pb)

#Now the same but for the buffers
buffers_natural_parks@data$ID <- as.factor(row.names(buffers_natural_parks@data))
list_polygons_buffers <- list()
total <- length(buffers_natural_parks@data$ID)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in buffers_natural_parks@data$ID){
  list_polygons_buffers[[i]] <- buffers_natural_parks[buffers_natural_parks@data$ID == i, ]
  setTxtProgressBar(pb, i)
}
close(pb)
rm(total, pb)

######################## VERIFICATION: BUFFERS = POLYGONS ########################
id1 <-list()
for(i in c(1:length(natural_parks[[2]]@polygons))){
  
  id1[[i]] <- slot(natural_parks[[2]]@polygons[[i]], "ID")
}

id2 <-list()
for(i in c(1:length(buffers_natural_parks@polygons))){
  
  id2[[i]] <- slot(buffers_natural_parks@polygons[[i]], "ID")
}
identical(id1, id2)
rm(id1, id2)
################################################################################


#Clean polygons 

#Open shapefiles 
setwd("~/Dropbox/BANREP/Deforestacion/Datos")

#Open shapefiles (only keep those territories after 2012)
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015)")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015)") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% #Projection in meters
  lapply(., function(x){
    x@data <- mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x[!as.numeric(x@data$year) > 2012, ])
    })


#Prepare data
black_points <- territories_proj[[1]] %>% as("SpatialLines") %>% as("SpatialPoints")
indigenous_points <- territories_proj[[2]] %>% as("SpatialLines") %>% as("SpatialPoints") 
list_polygons_proj <- lapply(list_polygons, spTransform, CRS=CRS("+init=epsg:3857"))

#Eliminate close to water (from colombia_raster.R)
colombia_municipios_p <- colombia_municipios[[2]] %>% as("SpatialLines") %>% as("SpatialPoints")
 
#Create a hole-free shapefile (simplify work) [Roger Bivand great function to remove hole slot for each polygon]
territories_hole_free <- lapply(territories_proj, function(x){
  BCp <- slot(x, "polygons")
  holes <- lapply(BCp, function(y) sapply(slot(y, "Polygons"), slot, "hole")) 
  res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]]) 
  IDs <- row.names(x)
  SpatialPolygons(lapply(1:length(res), function(i) Polygons(res[[i]], ID=IDs[i])), proj4string = CRS(proj4string(x))) 
})
  

#Dissolve and merge all geometries to simplify borders 
territories_dissolve <- lapply(territories_hole_free, function(x){
  unionSpatialPolygons(x, c(1:length(x@polygons))) 
  })


#Clean SpatialPoints (from polygons of Natural parks) -remove other treatments and get effective boundaries-
clean_treatments <- function(x, polygon, points_sp, points_border){
  print(x$ID)
  if(gIntersects(x, polygon)){
    #Remove inside points
    dif <- gDifference(x, polygon, drop_lower_td = T)
    if(!is.null(dif)){
    dif <- tidy(dif)[, 1:2] #Coordinates difference
    polygon2_coords <- tidy(x)[,1:2] #Coordinates polygon
    # Duplicated_coords is the non-intersecting points of the polygon2
    duplicated_coords <- anti_join(dif, polygon2_coords) 
    res <- SpatialPoints(duplicated_coords)
    }
    #Remove close cofounding treatments
    knn <- get.knnx(coordinates(points_sp), coordinates(res), k = 1, algorithm = "kd_tree") %>%
      data.frame(.) 
    sp <- SpatialPointsDataFrame(res, knn) %>%
      .[!.@data$nn.dist < 2000, ]
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_final <- SpatialPointsDataFrame(sp, knn_border) %>%
      .[!.@data$nn.dist < 1000, ] 
    
  } else {
    #Remove close cofounding treatments
    points <- x %>% as("SpatialLines") %>% as("SpatialPoints")
    knn <- get.knnx(coordinates(points_sp), coordinates(points), k = 1, algorithm = "kd_tree") %>%
      data.frame() 
    sp <- SpatialPointsDataFrame(points, knn) %>%
      .[!.@data$nn.dist < 2000, ] 
      knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_final <- SpatialPointsDataFrame(sp, knn_border) %>%
      .[!.@data$nn.dist < 1000, ] 
  }
  
}

########################################## INDIVIDUAL CLEANING ################################################

#Clean natural parks from black communitary lands
list_polygons_clean <- lapply(list_polygons_proj, clean_treatments, polygon = territories_union[[1]],
                              points_sp = black_points, points_border = colombia_municipios_p)

#Clean natural parks from indigenous resguardos
list_polygons_clean_indigenous <- lapply(list_polygons_proj, clean_treatments, polygon = territories_union[[2]],
                                  points_sp = indigenous_points, points_border = colombia_municipios_p)


###############################################################################################################

#Clean natural parks from both (create the union between borth territories)
territories_merge <- territories_dissolve %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  })

territories_merge <- raster::union(territories_merge[[1]], territories_merge[[2]])
list_polygons_clean_all <- lapply(list_polygons_proj, clean_treatments, polygon = territories_merge,
                                         points_sp = territories_merge_p, points_border = colombia_municipios_p)

valid <- sapply(list_polygons_proj, function(x){
  gCovers(territories_merge, x)
})

# Does it work?
#Communitary black lands
plot(list_polygons_clean[[3]], border = "blue")
plot(list_polygons_proj[[3]], add = T)
plot(black_hole_free, add = T, col = "red")

plot(list_polygons_proj[[45]])
plot(list_polygons_clean[[45]], add = T, col = "blue")
plot(black_hole_free, add = T, border = "red")

plot(list_polygons_proj[[392]])
plot(territories_union[[1]], add = T, border = "red")
plot(territories_union[[2]], add = T, border = "orange")
plot(list_polygons_clean_all[[392]], add = T, col = "blue")

#Vecinos


plot(black_points, add = T, col = "red")
vecinos <- get.knnx(coordinates(black_points), coordinates(p41), k = 1, algorithm = "kd_tree") %>%
  data.frame()
sp <- SpatialPointsDataFrame(p41, vecinos)




# Remove overlay polygons
if(gIntersects(list_polygons_proj[[39]], black_hole_free)) {
  dif <- gDifference(list_polygons_proj[[93]], territories_merge)
  clip_coords <- fortify(dif)[, 1:2]          # or, clip@polygons[[1]]@Polygons[[1]]@coords
  polygon2_coords <- tidy(list_polygons_proj[[93]])[, 1:2]  # or, polygon2@polygons[[1]]@Polygons[[1]]@coords
  duplicated_coords <- anti_join(clip_coords, polygon2_coords)
  
  # duplicated_coords is the non-intersecting points of the polygon2
  plot(list_polygons_proj[[194]])
  plot(territories_union[[1]], add = T, border = "red")
  plot(territories_union[[2]], add = T, border = "orange")
  plot(dif, add = T, border = "blue")
  res <- SpatialPoints(duplicated_coords)
  plot(list_polygons_clean_all[[194]], col="red", pch=19, add=T)
  
  dif <- gDifference(res, territories_hole_free[[1]])
  clip_coords <- data.frame(lang = coordinates(dif)[,1], lat = coordinates(dif)[, 2])        # or, clip@polygons[[1]]@Polygons[[1]]@coords
  polygon2_coords <- data.frame(lang = coordinates(res)[,1], lat = coordinates(res)[, 2]) # or, polygon2@polygons[[1]]@Polygons[[1]]@coords
  duplicated_coords <- merge(clip_coords, polygon2_coords)
  
  #Remove neighbors in 1 km 
  p93 <- list_polygons_proj[[93]] %>% as("SpatialLines") %>% as("SpatialPoints")
  
  vecinos1 <- get.knnx(coordinates(territories_merge_p), coordinates(res), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
  sp <- SpatialPointsDataFrame(res, vecinos1) %>%
  .[!.$nn.dist < 1000, ]
  vecinos_border <- get.knnx(coordinates(colombia_municipios_p), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
  sp_final <- SpatialPointsDataFrame(sp, vecinos_border) %>%
  .[!.@data$nn.dist < 1000, ] 
  plot(sp_final, col = "blue", pch = 19, add = T)
  
} else {
  #Remove neighbors in 1 km 
  p12 <- list_polygons_proj[[12]] %>% as("SpatialLines") %>% as("SpatialPoints")
  vecinos <- get.knnx(coordinates(black_points), coordinates(p12), k = 1, algorithm = "kd_tree") %>%
    data.frame()
  sp <- SpatialPointsDataFrame(p12, vecinos)
  plot(sp[sp$nn.dist < 1000, ], add = T, col = "blue")
  
}


# Get natural park SpatialPolygon atributes by cell number
deforest_cells <- SpatialPoints(xyFromCell(res[[1]], 1:prod(dim(res[[1]]))), proj4string = CRS(proj4string(natural_parks[[1]])))
natural_parks_atrb <- deforest_cells %over% natural_parks[[1]]
natural_parks_atrb$ID <- row.names(natural_parks_atrb)
natural_parks_atrb <- natural_parks_atrb[complete.cases(natural_parks_atrb[]), ]

#Identify cells inside national parks and buffers and their identifier
cells_naturalparks <- cellFromPolygon(res[[1]], natural_parks[[1]])
cells_naturalparks_buffers <- cellFromPolygon(res[[1]], buffers_natural_parks)
cells <- mapply(function(x, y){ #Remove cells from natural park polygons and list only the buffer pixels
  x[! x %in% y]
}, x = cells_naturalparks_buffers, y = cells_naturalparks)

#Mask raster to values indices buffers
res_mask_natural_parks_buffers <- mask(res[[1]], buffers_natural_parks)

#Create SpatialPoints object to calculate distances from buffer cells to natural parks boundaries
list_polygons_p <- lapply(list_polygons, function(x){ 
  x %>%
    as("SpatialLines") %>%
    as("SpatialPoints")})

#Calculate distances (functional loop - use a mlaply if too slow)
rasterOptions(tmpdir = "/Volumes/LaCie/Deforestacion/Hansen/Temp")

calculate_distances_parallel <- function(buffer, points){
  if(length(points > 0)){
  crop(res_mask_natural_parks_buffers, buffer) %>%
    mask(buffer) %>%
    clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
    mask(buffer) %>%
    resample(res_mask_natural_parks_buffers)
  } else
  return(0)
}

beginCluster()
system.time(mask <- mapply(calculate_distances_parallel,
                           buffer = list_polygons_buffers, 
                           points = list_polygons_p))
endCluster()

stack_distances <- stack(mask)

###################################### EXTRACT #################################################
#1. Extract distance as data frame per buffer (list element)
list_dataframes <- pblapply(mask, as.data.frame, xy = T, na.rm = T)

#2. Extract row names (id cells)
list_dataframes <- pblapply(list_dataframes, function(x){
  x$ID <- row.names(x); x
})

#3. Append all elements of the list 
distance_dataframe <- do.call(rbind, list_dataframes)
distance_dataframe$buffer_id <- rep(names(list_dataframes), sapply(list_dataframes, nrow)) #identify cells from buffers

######################################## WARNING #############################################
# The number of cells identified previously using cellsFromPolygon it is lower than the      #
# lenght of the ID column vector in the data.frame. This is explained because the distance   #
# buffer rasters have more cells than the masked "res" raster -they treat as non-NA parts of #
# sea and other parts outside the colombian continental land.                                #
##############################################################################################

#4. Identify treatment and remove NA's (read WARNING)
distance_dataframe$treatment <- ifelse(distance_dataframe$ID %in% unlist(cells_naturalparks), 1, 0)
distance_dataframe <- filter(distance_dataframe, ID %in% unlist(cells_naturalparks_buffers))

#4. Extract deforestation brick (and remove NA's)
deforestation_dataframe <- raster::extract(res, seq_len(ncell(res)), df = T)
deforestation_dataframe <- deforestation_dataframe[complete.cases(deforestation_dataframe[2:length(deforestation_dataframe)]), ]

#Write CSV
write.csv(distance_dataframe, "distancia_dataframe.csv", row.names = F)













