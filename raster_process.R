library(raster)
library(rgdal)
library(magrittr)
library(sp)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(lattice)
library(plyr)
library(grid)
library(dplyr)
library(stringr)

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


############################################### NOT RUN ########################################################
#Calculate distances from cell centrids the the natural parks frontiers
natural_parks_p <- natural_parks_proj %>%
  as("SpatialLines") %>%
  as("SpatialPoints")

#As this process can take a lot of time, we can use cluster computing
beginCluster()

system.time(
  distance_raster <- clusterR(res[[1]], distanceFromPoints, args = list(xy = natural_parks_p))
)
endCluster()
################################################################################################################


#Buffers to asses "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000)
buffers_natural_parks <- spTransform(buffers_natural_parks_proj, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##Not run: verification that buffers and polygons coincide
id1 <-list()
for(i in c(1:length(natural_parks[[2]]@polygons))){
  
  id1[[i]] <- slot(natural_parks[[2]]@polygons[[i]], "ID")
}

id2 <-list()
for(i in c(1:length(buffers_natural_parks@polygons))){
  
  id2[[i]] <- slot(buffers_natural_parks@polygons[[i]], "ID")
}
identical(id1, id2)
#################################################################

# Get natural park SpatialPolygon atributes by cell number
deforest_cells <- SpatialPoints(xyFromCell(res[[1]], 1:prod(dim(res[[1]]))), proj4string = CRS(proj4string(natural_parks[[1]])))
natural_parks_atrb <- deforest_cells %over% natural_parks[[1]]
natural_parks_atrb$ID <- row.names(natural_parks_atrb)

#Identify cells inside national parks and buffers and their identifier
cells_naturalparks <- cellFromPolygon(res[[1]], natural_parks[[1]])
cells_naturalparks_buffers <- cellFromPolygon(res[[1]], buffers_natural_parks)
cells <- mapply(function(x, y){ #Remove cells from natural park polygons and list only the buffer pixels
  x[! x %in% y]
}, x = cells_naturalparks_buffers, y = cells_naturalparks)

#Mask raster to values indice buffers
res_mask_natural_parks <- mask(res[[1]], natural_parks[[1]])
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

#Discrete case
p1 <- crop(res_mask_natural_parks_buffers, list_polygons_buffers[[1]])
p1 <- mask(p1, list_polygons_buffers[[1]])

beginCluster()
system.time(p2 <- clusterR(p1, distanceFromPoints, args = list(xy = list_polygons_p[[1]])))
endCluster()

p3 <- mask(p2, list_polygons_buffers[[1]])
p4 <- merge(p3, res_mask_natural_parks_buffers)
p4 <- resample(p4, res_mask_natural_parks_buffers)

a <- nrow(res_mask_natural_parks_buffers) - nrow(p2)
b <- ncol(res_mask_natural_parks_buffers) - ncol(p2)
p3 <- extend(p2, c(a, b))

#Lists
calculate_distance <- function(){
  pts <- xyFromCell(res_mask_natural_parks_buffers, cell = n, spatial = T)
  proj4string(pts) <- CRS(proj4string(natural_parks[[2]]))
  return(as.vector(gDistance(pts, shp, byid = T)))
}


calculate_distances_parallel <- function(raster, buffer, points){
  clusterR(raster, mask, args = list(mask = buffer)) %>%
    clusterR(., distanceFromPoints, args = list(xy = points))
}


calculate_distances <- function(raster, buffer, points){
  mask(raster, buffer) %>%
    distanceFromPoints(., xy = points)
}



beginCluster()
system.time(mask <- mapply(calculate_distances_parallel, 
                           raster = res_mask_natural_parks_buffers, 
                           buffer = list_polygons_buffers[1:2], 
                           points = list_polygons_p[1:2]))
endCluster()

calculate_distances(raster = res_mask_natural_parks_buffers, 
                    buffer = list_polygons_buffers[[1]], 
                    points = list_polygons_p[[1]])



