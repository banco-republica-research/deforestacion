##############################################################################################
##############################################################################################
###                     STRATEGY 2: EFFECTIVE TREATMENTS AND CONTROLS                      ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                    (TERRITORIES)                                       ###
##############################################################################################
##############################################################################################
rm(list=ls())
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
library(pbapply)

# Load functions in R
setwd("~/deforestacion/")
source("R/process_rasters.R")
source("R/clean_polygons.R")
source("R/calculate_distances.R")
source("cleaning/colombia_continental.R")

# Set directories
data <- "Deforestacion/Datos/"
setwd("~/Dropbox/BANREP/")

#Get deforestation raster for reference: deforestation
res <- brick(paste0(data, "HansenProcessed/1.4/loss_year_brick_1km.tif"))


###############################################################################
######### READ SHAPEFILES: NATURAL PROTECTED AREAS AND ARRANGE DATA ###########
###############################################################################

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
natural_parks <- readOGR(dsn = paste0(data, "UNEP", "/", "WDPA_June2016_COL-shapefile"), 
                         layer = "WDPA_June2016_COL-shapefile-polygons",
                         stringsAsFactors = F)
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
                           "Uramba Bahia Malaga")) & !x@data$STATUS_YR > 2016 & !x@data$GIS_AREA < 1 , ]
    
    
  }) %>%
  #Remove sections of park outside continental Colombia
  mapply(function(x, y){
    raster::intersect(y, x)
  }, x = . , y = colombia_municipios)


###############################################################################
#################################### NOTE #####################################
# 1. FOR IDENTIFICATION PURPOSES, AN ID VARIABLE IS CREATED TO EACH POLYGON
# USING THE ROW.NUMBER.
# 2. THIS ID IS PASSED ALSO TO THE LIST ID - THAT WAY WE CAN IDENTIFY TO WHICH
# PARK THE DATA IS RELATED TO.
###############################################################################
###############################################################################

natural_parks[[2]]@data$ID <- c(1:dim(natural_parks[[2]])[1])
natural_parks[[1]]@data$ID <- c(1:dim(natural_parks[[1]])[1])


###############################################################################
###############################################################################



#Remove redundant geometries from black territories 

#Get union of polygons
natural_parks_indigenous_merge <- gUnion(territories_merge[[2]], natural_parks[[2]])
naturaLparks_black_merge <- raster::union(territories_merge[[1]], natural_parks[[2]])

natural_parks_indigenous_merge_p <- rbind(indigenous_points, naturalparks_points)
naturaLparks_black_merge_p <- rbind(black_points, naturalparks_points)


#Buffers to asses "treatment zones" of 50 km 
buffers_territories <- lapply(territories_proj, gBuffer, byid = T, width = 50000) %>% 
  lapply(., spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Mask raster to each shapefile
res_mask_buffers <- lapply(buffers_territories, function(x){
  mask(res[[1]], x)
})

#Identify cells from buffer and territory

cells_territories <- mapply(function(x, y){
  cellFromPolygon(res[[1]], y[y@data$OBJECTID_1 %in% x@data$OBJECTID_1,])
}, x = territories_proj , y = territories)

cells_territories_buffers <- lapply(buffers_territories, function(x){
  cellFromPolygon(res[[1]], x)
})

list_polygons_buffers <- lapply(buffers_territories, polygon_to_list)
list_polygons_territories <- lapply(territories_proj, polygon_to_list)

#Correct list of polygons (for topo problems)
list_polygons_territories <- list_polygons_territories %>%
  rapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  }, how = "replace")

# territories_proj_corrected <- territories_proj %>%
#   lapply(function(x){
#     gBuffer(x, byid = T, width = 0) %>%
#       gSimplify(., tol = 0.001)
#   })


#Clean polygons
list_polygons_clean_black_all <- lapply(list_polygons_territories[[1]], clean_treatments, polygon = natural_parks_indigenous_merge,
                                        points_sp = natural_parks_indigenous_merge_p, points_border = colombia_municipios_p,
                                        shape = territories_proj_corrected[[1]])
saveRDS(list_polygons_clean_black_all, "list_polygons_clean_black_all.rds")

list_polygons_clean_indigenous_all <- lapply(list_polygons[[2]], clean_treatments, polygon = naturaLparks_black_merge,
                                             points_sp = naturaLparks_black_merge_p, points_border = colombia_municipios_p,
                                             shape = territories_proj_corrected[[2]])
saveRDS(list_polygons_clean_indigenous_all, "list_polygons_clean_indigenous_all.rds")




p <- clean_treatments(list_polygons[[2]][[215]], polygon = naturaLparks_black_merge,
                      points_sp = naturaLparks_black_merge_p, points_border = colombia_municipios_p,
                      shape = territories_proj_corrected[[2]])

#Does it work? 
plot(list_polygons[[2]][[137]])
plot(naturaLparks_black_merge, add = T, border = "red")
plot(list_polygons_clean_indigenous_all[[1]], add = T, col = "blue", pch = 19)



list_clean <- list(list_polygons_clean_black_all, list_polygons_clean_indigenous_all)

#Reproject list to raster projection (WGS84)
list_polygon_clean_proj <- rapply(list_clean, function(x){
  if(typeof(x) == "S4" & length(x) > 0){
    sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(sp)
  } else { 
    return(0)
  }
}, how = "replace")


calculate_distances_parallel <- function(buffer, points){
  if(length(points)  > 2 & typeof(points) == "S4"){
    crop(res_mask_buffers[[1]], buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_buffers[[2]])
  } else
    return(0)
}


beginCluster()
system.time(distances_black <- mapply(calculate_distances_parallel,
                                      buffer = list_polygons_buffers[[1]], 
                                      points = list_polygons_clean_proj[[1]]))
endCluster()

beginCluster()
system.time(distances_indigenous <- mapply(calculate_distances_parallel,
                                           buffer = list_polygons_buffers[[2]], 
                                           points = list_polygon_clean_proj[[2]]))
endCluster()

###################################### EXTRACT #################################################
#1. Extract distance as data frame per buffer (list element)
list_dataframes_black <- pblapply(distances_black, as.data.frame, xy = T, na.rm = T)
list_dataframes_indigenous <- pblapply(distances_indigenous, as.data.frame, xy = T, na.rm = T)

#2. Extract row names (id cells)
list_dataframes_black <- pblapply(list_dataframes_black, function(x){
  x$ID <- row.names(x); x
})


list_dataframes_indigenous <- pblapply(list_dataframes_indigenous, function(x){
  x$ID <- row.names(x); x
})

zero_lenght <- unname(which(sapply(list_dataframes_black, function(x) dim(x)[2] != 4)))
for(i in zero_lenght){
  list_dataframes_black[[i]] <- data.frame(x = c(0), y = c(0), layer = c(0), ID = c(0))
}

zero_lenght <- unname(which(sapply(list_dataframes_indigenous, function(x) dim(x)[2] != 4)))
for(i in zero_lenght){
  list_dataframes_indigenous[[i]] <- data.frame(x = c(0), y = c(0), layer = c(0), ID = c(0))
}

#3. Append all elements of the list 
distance_dataframe_black <- do.call(rbind, list_dataframes_black)
distance_dataframe_black$buffer_id <- rep(names(list_dataframes_black), sapply(list_dataframes_black, nrow)) #identify cells from buffers

distance_dataframe_indigenous <- do.call(rbind, list_dataframes_indigenous)
distance_dataframe_indigenous$buffer_id <- rep(names(list_dataframes_indigenous), sapply(list_dataframes_indigenous, nrow)) #identify cells from buffers

#4. Identify treatment and remove NA's (read WARNING)
distance_dataframe_black$treatment <- ifelse(distance_dataframe_black$ID %in% unlist(cells_territories[[1]]), 1, 0)
distance_dataframe_indigenous$treatment <- ifelse(distance_dataframe_indigenous$ID %in% unlist(cells_territories[[2]]), 1, 0)

list <- list(distance_dataframe_black, distance_dataframe_indigenous)
names(list) <- c("black", "indigenous")

#5. Export
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 2/")
lapply(1:length(list), function(i){
  write.csv(list[[i]], file = str_c("distance_dataframe_", names(list)[[i]], ".csv") , row.names = FALSE)
})
