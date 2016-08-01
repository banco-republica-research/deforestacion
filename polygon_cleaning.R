##############################################################################################
##############################################################################################
###                                                                                        ###
###                                         TEST                                           ###
###                                                                                        ###
##############################################################################################
##############################################################################################

library(rgeos)
library(sp)

#Create SpatialPlygons objects
polygon1 <- readWKT("POLYGON((-190 -50, -200 -10, -110 20, -190 -50))")          
#polygon 1
polygon2 <- readWKT("POLYGON((-180 -20, -140 55, 10 0, -140 -60, -180 -20))") #polygon 2

#Plot both polygons
par(mfrow = c(1,2)) #in separate windows
plot(polygon1, main = "Polygon1") #window 1
plot(polygon2, main = "Polygon2") #window 2

polygon_set <- readWKT(paste("POLYGON((-180 -20, -140 55, 10 0, -140 -60, -180 -20),",
                             "(-190 -50, -200 -10, -110 20, -190 -50))"))

par(mfrow = c(1,1)) #now, simultaneously
plot(polygon_set, main = "Polygon1 & Polygon2")


clip <- gDifference(polygon2, polygon1, byid = TRUE, drop_lower_td = T) #clip polygon 2 with polygon 1
plot(clip, col = "red", add = T)

library(ggplot2)                         # as @shayaa commented, ggplot2::fortify is useful.

clip_coords <- fortify(clip)[,1:2]          # or, clip@polygons[[1]]@Polygons[[1]]@coords
polygon2_coords <- fortify(polygon2)[,1:2]  # or, polygon2@polygons[[1]]@Polygons[[1]]@coords
duplicated_coords <- merge(clip_coords, polygon2_coords) 
# duplicated_coords is the non-intersecting points of the polygon2
res <- SpatialPoints(duplicated_coords)

plot(clip)
plot(res, col="red", pch=19, add=T)



##################################### CORRECTION ###########################################

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
library(stringr)

setwd("/Volumes/LaCie/Deforestacion/Hansen")
res <- brick("loss_year_brick_1km.tif")

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
natural_parks <- readOGR(dsn = "WDPA_June2016_COL-shapefile", layer = "WDPA_June2016_COL-shapefile-polygons")
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Remove NP that are out of continental land (Malpelo and Providencia)
natural_parks <- list(natural_parks, natural_parks_proj) %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park")), ]
  })


#Buffers to asses "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000)
buffers_natural_parks <- spTransform(buffers_natural_parks_proj, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Clean polygons

#Create a hole-free shapefile (simplify work)
BCp <- slot(territories_proj[[1]], "polygons")
holes <- lapply(BCp, function(x) sapply(slot(x, "Polygons"), slot, "hole")) 
res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]]) 
IDs <- row.names(territories_proj[[1]]) 
black_hole_free <- SpatialPolygons(lapply(1:length(res), function(i) Polygons(res[[i]], ID=IDs[i])), proj4string=CRS(proj4string(territories_proj[[1]]))) 



list_polygons_proj <- lapply(list_polygons, spTransform, CRS=CRS("+init=epsg:3857"))
black_points <- territories_proj[[1]] %>% as("SpatialLines") %>% as("SpatialPoints")

list_polygons_clean <- lapply(list_polygons_proj, function(x){
  #Remove inside points
  dif <- gDifference(x, black_hole_free, drop_lower_td = T) %>%
    fortify(dif)[,1:2] #Coordinates difference
  polygon2_coords <- fortify(x)[,1:2] #Coordinates polygon
  # Duplicated_coords is the non-intersecting points of the polygon2
  duplicated_coords <- merge(dif, polygon2_coords) 
  res <- SpatialPoints(duplicated_coords)
  
  #Remove close cofounding treatments
  knn <- get.knnx(coordinates(black_points), coordinates(res), k = 1, algorithm = "kd_tree") %>%
    data.frame() 
  sp <- SpatialPointsDataFrame(x, knn) %>%
    .[!.@data$nn.dist < 1000]
})

#Examples: Utria
plot(list_polygons_proj[[41]])
plot(p41, add = T)
plot(black_points, add = T, col = "red")
vecinos <- get.knnx(coordinates(black_points), coordinates(p41), k = 1, algorithm = "kd_tree") %>%
  data.frame()
sp <- SpatialPointsDataFrame(p41, vecinos)


#Examples: Río León
plot(list_polygons_proj[[351]])
p351 <- list_polygons_proj[[351]] %>% as("SpatialLines") %>% as("SpatialPoints")
plot(p351, add = T)
plot(black_points, add = T, col = "red")
vecinos <- get.knnx(coordinates(black_points), coordinates(p351), k = 1, algorithm = "kd_tree") %>%
  data.frame()
sp <- SpatialPointsDataFrame(p351, vecinos)
plot(sp[sp$nn.dist < 1000, ], add = T, col = "blue")


dif <- gDifference(list_polygons_proj[[41]], black_hole_free, byid = T, drop_lower_td = T)
plot(list_polygons_proj[[41]])
plot(black_hole_free, add = T, border = "red")
plot(dif, border = "blue", add = T)


clip_coords <- fortify(dif)[,1:2]          # or, clip@polygons[[1]]@Polygons[[1]]@coords
polygon2_coords <- fortify(list_polygons_proj[[41]])[,1:2]  # or, polygon2@polygons[[1]]@Polygons[[1]]@coords
duplicated_coords <- merge(clip_coords, polygon2_coords) 
# duplicated_coords is the non-intersecting points of the polygon2
res <- SpatialPoints(duplicated_coords)
plot(res, col="red", pch=19, add=T)

#Example: Río León (función)
plot(list_polygons_proj[[351]])
plot(as(as(list_polygons_proj[[351]], "SpatialLines"), "SpatialPoints"), add = T)
plot(black_points, add = T, col = "red")
plot(list_polygons_clean[[351]], add = T, col = "blue")


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
################################################################################

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

#Create SpatialPoints object to calculate distances from buffer cells to natural parks boundaries
list_polygons_p <- lapply(list_polygons, function(x){ 
  x %>%
    as("SpatialLines") %>%
    as("SpatialPoints")})

#Calculate distances (functional loop - use a mlaply if too slow)
rasterOptions(tmpdir = "/Volumes/LaCie/Deforestacion/Hansen/Temp")

calculate_distances_parallel <- function(buffer, points){
  crop(res_mask_natural_parks_buffers, buffer) %>%
    mask(buffer) %>%
    clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
    mask(buffer) %>%
    resample(res_mask_natural_parks_buffers)
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













