##############################################################################################
##############################################################################################
###               STRATEGY 1: BAD TREATMENTS AND CONTROLS WITHIN COLOMBIA                  ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                  (NATURAL PARKS)                                       ###
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
library(pbapply)
library(FNN)
library(dplyr)
library(broom)
library(stringr)

# Load functions in R
setwd("~/deforestacion/")
source("R/process_rasters.R")
source("R/calculate_distances.R")
source("cleaning/colombia_continental.R")

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

res <- brick("HansenProcessed/1.4/loss_year_brick_1km.tif")


#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
natural_parks <- readOGR(dsn = paste0("UNEP/WDPA_June2016_COL-shapefile"), 
                         layer = "WDPA_June2016_COL-shapefile-polygons", stringsAsFactors = F)
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
  }) 
# %>%
#   #Remove sections of park outside continental Colombia
#   mapply(function(x, y){
#     raster::intersect(y, x)
#   }, x = . , y = colombia_municipios) 

#For tracktability
natural_parks[[2]]@data$ID <- c(1:dim(natural_parks[[2]])[1])
natural_parks[[1]]@data$ID <- c(1:dim(natural_parks[[1]])[1])

# Write metadata to dta (run panels in Stata)
write.dta(natural_parks[[1]]@data, paste0("UNEP", "/", "natural_parks_strategy1.dta"))

#Buffers to asses "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Create a list of individual polygons per natural park
list_polygons <- list()
total <- length(natural_parks[[1]]@data$ID)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in natural_parks[[1]]@data$ID){
  list_polygons[[i]] <- natural_parks[[1]][natural_parks[[1]]@data$ID == i, ]
  setTxtProgressBar(pb, i)
}
close(pb)

#Now the same but for the buffers
list_polygons_buffers <- list()
total <- length(buffers_natural_parks_proj@data$ID)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in buffers_natural_parks_proj@data$ID){
  list_polygons_buffers[[i]] <- buffers_natural_parks_proj[buffers_natural_parks_proj@data$ID == i, ]
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
for(i in c(1:length(buffers_natural_parks_proj@polygons))){
  
  id2[[i]] <- slot(buffers_natural_parks_proj@polygons[[i]], "ID")
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

# Does it work? (Yaigoje Apaporis)
plot(list_polygons[[10]])
plot(list_polygons_clean_border_proj[[10]], col="red", pch=19, add=T)


#Identify cells inside national parks and buffers and their identifier
cells_naturalparks <- cellFromPolygon(res[[1]], natural_parks[[1]])
cells_naturalparks_buffers <- cellFromPolygon(res[[1]], buffers_natural_parks_proj)
cells <- mapply(function(x, y){ #Remove cells from natural park polygons and list only the buffer pixels
  x[! x %in% y]
}, x = cells_naturalparks_buffers, y = cells_naturalparks)

#Mask raster to values indices buffers
res_mask_natural_parks_buffers <- mask(res[[1]], buffers_natural_parks_proj)

#Calculate distances (functional loop - use a mlaply if too slow)

calculate_distances_parallel <- function(buffer, points){
    crop(res_mask_natural_parks_buffers, buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_natural_parks_buffers)
}

beginCluster()
system.time(mask_distance_border <- mapply(calculate_distances_parallel,
                                    buffer = list_polygons_buffers, 
                                    points = list_polygons_clean_border_proj))
endCluster()

stack_distances <- stack(mask_distance_border)
saveRDS(stack_distances, "rds_data/distances_border.rds")
###################################### EXTRACT #################################################
#1. Extract distance as data frame per buffer (list element)
list_dataframes_border <- pblapply(mask_distance_border, as.data.frame, xy = T, na.rm = T)

#2. Extract row names (id cells)
list_dataframes <- pblapply(list_dataframes_border, function(x){
  x$ID <- row.names(x); x
})

#3. Append all elements of the list 
distance_dataframe <- do.call(rbind, list_dataframes)
distance_dataframe$buffer_id <- rep(c(1:length(list_dataframes)), sapply(list_dataframes, nrow)) #identify cells from buffers


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

write.csv(distance_dataframe, "Dataframes/Estrategia 1/distancia_dataframe_borders.csv", row.names = F)
distance_dataframe_borders <- read.csv("distancia_dataframe_borders.csv")


##############################################################################################
##############################################################################################
###               STRATEGY 1: BAD TREATMENTS AND CONTROLS WITHIN COLOMBIA                  ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                    (TERRITORIES)                                       ###
##############################################################################################
##############################################################################################


#Open shapefiles (only keep those territories after 2012)
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015)")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015)") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% #Projection in meters
  lapply(., function(x){
    x@data <- mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x[!as.numeric(x@data$year) > 2016, ])
  }) %>%
  lapply(.,function(x){
    x$ID <- c(1:length(x))
    return(x)
  })


writeOGR(obj = territories_proj[[1]], dsn = "Comunidades" , layer = "Comunidades_clean", driver = "ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(obj = territories_proj[[2]], dsn = "Resguardos" , layer = "Resguardos_clean", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#Buffers to asses "treatment zones" of 50 km 
buffers_territories <- lapply(territories_proj, gBuffer, byid = T, width = 50000) %>% 
  lapply(., spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Mask raster to each shapefile
res_mask_buffers <- lapply(buffers_territories, function(x){
  mask(res[[1]], x)
})

#Identify cells from buffer and territory
cells_territories <- lapply(territories_proj, function(x){
  cellFromPolygon(res[[1]], x)
})

cells_territories_buffers <- lapply(buffers_territories, function(x){
  cellFromPolygon(res[[1]], x)
})

#Create a list of individual polygons per territory. 
polygon_to_list <- function(shape){
  list_polygons <- list()
  for(i in shape@data$ID){
    list_polygons[[i]] <- shape[shape@data$ID == i, ]
  }
  return(list_polygons)
}

list_polygons <- lapply(territories_proj, polygon_to_list)
list_polygons_buffers <- lapply(buffers_territories, polygon_to_list)

#Clean SpatialPoints (from polygons of Natural parks) -remove points out of national frontiers and border points-
clean_treatments_border <- function(x, points_border){
  print(x$ID)
  sp <- x %>% as("SpatialLines") %>% as("SpatialPoints")
  knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
  sp_final <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
    .[!.@data$nn.dist < 1500, ] 
  
}

list_polygons_clean_border_black <- lapply(list_polygons[[1]], clean_treatments_border, points_border = colombia_municipios_p)
list_polygons_clean_border_indigenous <- lapply(list_polygons[[2]], clean_treatments_border, points_border = colombia_municipios_p)


#Does it works?
plot(list_polygons[[2]][[172]])
plot(list_polygons_clean_border_indigenous[[172]], add = T, pch = 19, col = "red")

plot(list_polygons[[1]][[116]])
plot(list_polygons_clean_border_black[[116]], add = T, pch = 19, col = "red")



#Reproject points to calculate distances
list_polygons_clean_black_proj <- lapply(list_polygons_clean_border_black, function(x){
  if(length(x) > 0){
  sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(sp)
  }
  else{
    return(0)
  }
})

list_polygons_clean_indigenous_proj <- lapply(list_polygons_clean_border_indigenous, function(x){
  if(length(x) > 0){
    sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(sp)
  }
  else{
    return(0)
  }
})



#Get distance rasters for both territories

calculate_distances_parallel <- function(buffer, points){
  if(length(points) > 1){
  print(buffer@data$ID)
  crop(res_mask_buffers[[1]], buffer) %>%
    mask(buffer) %>%
    clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
    mask(buffer) %>%
    resample(res_mask_buffers[[1]])
  } else {
    return(0)
  }
}


beginCluster()
system.time(distances_black <- mapply(calculate_distances_parallel,
                                      buffer = list_polygons_buffers[[1]], 
                                      points = list_polygons_clean_black_proj))
endCluster()

saveRDS(distances_black, "rds_data/distances_black_border_2016.rds")

calculate_distances_parallel <- function(buffer, points){
  if(length(points) > 1){
    print(buffer@data$ID)
    crop(res_mask_buffers[[2]], buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_buffers[[2]])
  } else {
    return(0)
  }
}


beginCluster()
system.time(distances_indigenous <- mapply(calculate_distances_parallel,
                                           buffer = list_polygons_buffers[[2]], 
                                           points = list_polygons_clean_indigenous_proj))
endCluster()

saveRDS(distances_indigenous, "rds_data/distances_indigenous_border_2016.rds")


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


# 2.1. Correct polygons without distances (due to be too small)
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
distance_dataframe_black$buffer_id <- rep(c(1:length(list_dataframes_black)), sapply(list_dataframes_black, nrow)) #identify cells from buffers


distance_dataframe_indigenous <- do.call(rbind, list_dataframes_indigenous)
distance_dataframe_indigenous$buffer_id <- rep(c(1:length(list_dataframes_indigenous)), sapply(list_dataframes_indigenous, nrow)) #identify cells from buffers


#4. Identify treatment and remove NA's (read WARNING)
distance_dataframe_black$treatment <- ifelse(distance_dataframe_black$ID %in% unlist(cells_territories[[1]]), 1, 0)
distance_dataframe_indigenous$treatment <- ifelse(distance_dataframe_indigenous$ID %in% unlist(cells_territories[[2]]), 1, 0)

list <- list(distance_dataframe_black, distance_dataframe_indigenous)
names(list) <- c("black", "indigenous")

#5. Export

lapply(1:length(list), function(i){
  write.csv(list[[i]], file = str_c("Dataframes/Estrategia 1/df_until_2016/distance_dataframe_", names(list)[[i]], ".csv") , row.names = FALSE)
})



