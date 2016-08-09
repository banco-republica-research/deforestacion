##############################################################################################
##############################################################################################
###               STRATEGY 1: BAD TREATMENTS AND CONTROLS WITHIN COLOMBIA                  ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                  (NATURAL PARKS)                                       ###
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


#Prepare data
list_polygons_proj <- lapply(list_polygons, spTransform, CRS=CRS("+init=epsg:3857"))

#Eliminate close to water (from colombia_raster.R)
colombia_municipios_p <- colombia_municipios[[2]] %>% as("SpatialLines") %>% as("SpatialPoints")


#Clean SpatialPoints (from polygons of Natural parks) -remove points out of national frontiers and border points-
clean_treatments_border <- function(x, points_border){
  print(x$ID)
  sp <- x %>% as("SpatialLines") %>% as("SpatialPoints")
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_final <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ] 
    
}

list_polygons_clean_border <- lapply(list_polygons_proj, clean_treatments_border, points_border = colombia_municipios_p)


#Reproject list to raster projection (WGS84)
list_polygons_clean_border_proj <- lapply(list_polygons_clean_border, function(x){
    sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(sp)
})

# Does it work?
plot(list_polygons[[35]])
plot(list_polygons_clean_border_proj[[35]], col="red", pch=19, add=T)


#Identify cells inside national parks and buffers and their identifier
cells_naturalparks <- cellFromPolygon(res[[1]], natural_parks[[1]])
cells_naturalparks_buffers <- cellFromPolygon(res[[1]], buffers_natural_parks)
cells <- mapply(function(x, y){ #Remove cells from natural park polygons and list only the buffer pixels
  x[! x %in% y]
}, x = cells_naturalparks_buffers, y = cells_naturalparks)

# Get natural park SpatialPolygon atributes by cell number
deforest_cells <- SpatialPoints(xyFromCell(res[[1]], 1:prod(dim(res[[1]]))), proj4string = CRS(proj4string(natural_parks[[1]])))
natural_parks_atrb <- deforest_cells %over% natural_parks[[1]]
natural_parks_atrb$ID <- row.names(natural_parks_atrb)
natural_parks_atrb <- natural_parks_atrb[complete.cases(natural_parks_atrb[]), ]

setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
write.csv(natural_parks_atrb, "natural_parks_atrb.csv")

#Mask raster to values indices buffers
res_mask_natural_parks_buffers <- mask(res[[1]], buffers_natural_parks)

#Calculate distances (functional loop - use a mlaply if too slow)
rasterOptions(tmpdir = "/Volumes/LaCie/Deforestacion/Hansen/Temp")

calculate_distances_parallel <- function(buffer, points){
  if(length(points)  > 2 & typeof(points) == "S4"){
    crop(res_mask_natural_parks_buffers, buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_natural_parks_buffers)
  } else
    return(0)
}

beginCluster()
system.time(mask_distance_border <- mapply(calculate_distances_parallel,
                                    buffer = list_polygons_buffers, 
                                    points = list_polygons_clean_border_proj))
endCluster()

stack_distances <- stack(mask_distance_border)

###################################### EXTRACT #################################################
#1. Extract distance as data frame per buffer (list element)
list_dataframes_border <- pblapply(mask_distance_border, as.data.frame, xy = T, na.rm = T)

#2. Extract row names (id cells)
list_dataframes <- pblapply(list_dataframes_border, function(x){
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
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 1/")
write.csv(distance_dataframe, "distancia_dataframe_borders.csv", row.names = F)


##############################################################################################
##############################################################################################
###               STRATEGY 1: BAD TREATMENTS AND CONTROLS WITHIN COLOMBIA                  ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                    (TERRITORIES)                                       ###
##############################################################################################
##############################################################################################

#Remove redundant geometries from black territories 
natural_parks_indigenous_merge <- raster::intersect(territories_merge[[2]], natural_parks[[2]])






