

#Open shapefiles 
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Buffers to asses "treatment zones" of 50 km 
buffers_territories <- lapply(territories_proj, gBuffer, byid = T, width = 50000) %>%
  lapply(., spTransform, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Mask raster to each shapefile
res_mask_buffers <- lapply(buffers_territories, function(x){
  mask(res[[1]], x)
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





