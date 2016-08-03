

setwd("~/Dropbox/BANREP/Deforestacion/Datos")

#Open shapefiles 
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% #Projection in meters
  lapply(., function(x){
    x@data <- mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x)
  })




#Buffers to asses "treatment zones" of 50 km 
buffers_territories <- lapply(territories_proj, gBuffer, byid = T, width = 50000) %>%
  lapply(., spTransform, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Mask raster to each shapefile
res_mask_buffers <- lapply(buffers_territories, function(x){
  mask(res[[1]], x)
})

#Identify cells from buffer and territory
cells_territories <- lapply(territories, function(x){
  cellFromPolygon(res[[1]], x)
  })

cells_territories_buffers <- lapply(buffers_territories, function(x){
  cellFromPolygon(res[[1]], x)
})

#Create a list of individual polygons per territory. 
polygon_to_list <- function(shape){
  shape@data$ID <- as.factor(row.names(shape@data))
  for(i in shape@data$OBJECTID_1){
  list_polygons[[i]] <- shape[shape@data$OBJECTID_1 == i, ]
}
  return(list_polygons)
  }

list_polygons <- lapply(territories, polygon_to_list)
list_polygons_buffers <- lapply(buffers_territories, polygon_to_list)

#Create SpatialPoints object to calculate distances from buffer cells to natural parks boundaries
list_polygons_p <- rapply(list_polygons, function(x){
  x %>%
    as("SpatialLines") %>%
    as("SpatialPoints")
} , how = "replace")
    
calculate_distances_parallel <- function(buffer, points){
  crop(res_mask_buffers[[2]], buffer) %>%
    mask(buffer) %>%
    clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
    mask(buffer) %>%
    resample(res_mask_buffers[[2]])
}

beginCluster()
system.time(distances_black <- mapply(calculate_distances_parallel,
                           buffer = list_polygons_buffers[[1]], 
                           points = list_polygons_p[[1]]))
endCluster()

beginCluster()
system.time(distances_indigenous <- mapply(calculate_distances_parallel,
                                      buffer = list_polygons_buffers[[2]], 
                                      points = list_polygons_p[[2]]))
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

#3. Append all elements of the list 
distance_dataframe_black <- do.call(rbind, list_dataframes_black)
names(list_dataframes_black) <- c(1: length(list_dataframes_black))
distance_dataframe_black$buffer_id <- rep(names(list_dataframes_black), sapply(list_dataframes_black, nrow)) #identify cells from buffers

distance_dataframe_indigenous <- do.call(rbind, list_dataframes_indigenous)
names(list_dataframes_indigenous) <- c(1: length(list_dataframes_indigenous))
distance_dataframe_indigenous$buffer_id <- rep(names(list_dataframes_indigenous), sapply(list_dataframes_indigenous, nrow)) #identify cells from buffers

#4. Identify treatment and remove NA's (read WARNING)
distance_dataframe_black$treatment <- ifelse(distance_dataframe_black$ID %in% unlist(cells_territories[[1]]), 1, 0)
distance_dataframe_indigenous$treatment <- ifelse(distance_dataframe_indigenous$ID %in% unlist(cells_territories[[2]]), 1, 0)

list <- list(distance_dataframe_black, distance_dataframe_indigenous)
names(list) <- c("black", "indigenous")

#5. Export
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
lapply(1:length(list), function(i){
  write.csv(list[[i]], file = str_c("distance_dataframe_", names(list)[[i]], ".csv") , row.names = FALSE)
})



