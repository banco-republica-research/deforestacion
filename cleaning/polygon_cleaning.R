##############################################################################################
##############################################################################################
###                     STRATEGY 2: EFFECTIVE TREATMENTS AND CONTROLS                      ###
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
library(pbapply)

setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
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

#For tracktability
natural_parks[[2]]@data$ID <- c(1:dim(natural_parks[[2]])[1])
natural_parks[[1]]@data$ID <- c(1:dim(natural_parks[[1]])[1])


#Buffers to asses "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000)
buffers_natural_parks <- spTransform(buffers_natural_parks_proj, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Create a list of individual polygons per natural park
list_polygons <- list()
total <- length(natural_parks[[2]]@data$ID)
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in natural_parks[[2]]@data$ID){
  list_polygons[[i]] <- natural_parks[[2]][natural_parks[[2]]@data$ID == i, ]
  setTxtProgressBar(pb, i)
}
close(pb)

#Now the same but for the buffers
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

#Open shapefiles territories
setwd("~/Dropbox/BANREP/Deforestacion/Datos")

#Open shapefiles (only keep those territories after 2012 and territories inside Colombian territory)
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015)")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015)") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% #Projection in meters
  lapply(., function(x){
    x@data <- mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x[!as.numeric(x@data$year) > 2012, ])
  })  %>%
  #Remove sections of park outside continental Colombia
  lapply(., function(x){
    gIntersection(x, colombia_municipios[[2]], byid = T, drop_lower_td = T)
  }) %>%
  lapply(.,function(x){
    x$ID <- c(1:length(x))
    return(x)
  })


#Prepare data
black_points <- territories_proj[[1]] %>% as("SpatialLines") %>% as("SpatialPoints")
indigenous_points <- territories_proj[[2]] %>% as("SpatialLines") %>% as("SpatialPoints") 
naturalparks_points <- natural_parks[[2]] %>% as("SpatialLines") %>% as("SpatialPoints")

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
  

#Dissolve and merge all geometries to simplify borders (eliminate subgeometries)
territories_dissolve <- lapply(territories_hole_free, function(x){
  unionSpatialPolygons(x, c(1:length(x@polygons))) 
  })

territories_union <- lapply(territories_dissolve, gUnaryUnion)


#Clean SpatialPoints (from polygons of Natural parks) -remove other treatments and get effective boundaries-
clean_treatments <- function(x, polygon, points_sp, points_border, shape){
  print(x$ID)
  if(gIntersects(x, polygon)){
    #Remove inside points
    dif <- gDifference(x, polygon, drop_lower_td = T)
    if(!is.null(dif)){
      dif <- tidy(dif)[, 1:2] #Coordinates difference
      polygon2_coords <- tidy(x)[,1:2] #Coordinates polygon
      # Duplicated_coords is the non-intersecting points of the polygon2
      duplicated_coords <- merge(dif, polygon2_coords) 
      if(dim(duplicated_coords)[1] > 0){
        res <- SpatialPoints(duplicated_coords, proj4string = CRS("+init=epsg:3857"))
      } else {
        res <- SpatialPoints(polygon2_coords, proj4string = CRS("+init=epsg:3857"))
      }
      
    } else {
      return(0)
    }
    #Remove close cofounding treatments
    knn <- get.knnx(coordinates(points_sp), coordinates(res), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
    sp <- SpatialPointsDataFrame(res, knn, proj4string = CRS("+init=epsg:3857")) %>%
    .[!.@data$nn.dist < 1000, ]
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
    sp_border <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
    .[!.@data$nn.dist < 1000, ]
    dif <- gDifference(shape, x) %>% as("SpatialLines") %>% as("SpatialPoints")
    knn_final <- get.knnx(coordinates(dif), coordinates(sp_border), k = 1, algorithm = "kd_tree") %>%
    data.frame()
    sp_final <- SpatialPointsDataFrame(sp_border, knn_final, proj4string = CRS("+init=epsg:3857")) %>%
    .[!.@data$nn.dist < 500, ]
    
  } else {
    # Remove close cofounding treatments
    points <- x %>% as("SpatialLines") %>% as("SpatialPoints")
    knn <- get.knnx(coordinates(points_sp), coordinates(points), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp <- SpatialPointsDataFrame(points, knn, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_border <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    dif <- gDifference(shape, x) %>% as("SpatialLines") %>% as("SpatialPoints")
    knn_final <- get.knnx(coordinates(dif), coordinates(sp_border), k = 1, algorithm = "kd_tree") %>%
      data.frame()
    sp_final <- SpatialPointsDataFrame(sp_border, knn_final, proj4string = CRS("+init=epsg:3857")) %>%
       .[!.@data$nn.dist < 500, ]
  }
  
}

########################################## INDIVIDUAL CLEANING ################################################
############################################### (NOT RUN) #####################################################
#Clean natural parks from black communitary lands
list_polygons_clean <- lapply(list_polygons_proj, clean_treatments, polygon = territories_union[[1]],
                              points_sp = black_points, points_border = colombia_municipios_p)

#Clean natural parks from indigenous resguardos
list_polygons_clean_indigenous <- lapply(list_polygons_proj, clean_treatments, polygon = territories_union[[2]],
                                  points_sp = indigenous_points, points_border = colombia_municipios_p)


###############################################################################################################


#Clean natural parks from both (create the union between borth territories)
territories_merge <- territories_union %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  })

natural_parks_corrected <- natural_parks %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  })


territories_merge <- raster::union(territories_merge[[1]], territories_merge[[2]])
territories_merge_p <- rbind(black_points, indigenous_points)


list_polygons_clean_all <- lapply(list_polygons, clean_treatments, polygon = territories_merge,
                                         points_sp = territories_merge_p, points_border = colombia_municipios_p,
                                  shape = natural_parks_corrected[[2]])
setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(list_polygons_clean_all, "list_polygons_clean_all.rds")



#Reproject list to raster projection (WGS84)
list_polygons_clean_all_proj <- lapply(list_polygons_clean_all, function(x){
  if(typeof(x) == "S4" & length(x) > 0){
    sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(sp)
  } else { 
  return(0)
  }
})
  
# Does it work?
chiribiquete_clean <- clean_treatments(list_polygons[[8]], polygon = territories_merge,
                                       points_sp = territories_merge_p, 
                                       points_border = colombia_municipios_p,
                                       shape = natural_parks_corrected[[2]])
plot(list_polygons[[8]])
plot(chiribiquete_clean, add = T, col = red)




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
system.time(mask_distance <- mapply(calculate_distances_parallel,
                           buffer = list_polygons_buffers, 
                           points = list_polygons_clean_all_proj))
endCluster()

stack_distances_all <- stack(mask_distance)

###################################### EXTRACT #################################################
#1. Extract distance as data frame per buffer (list element)
list_dataframes <- pblapply(mask_distance, as.data.frame, xy = T, na.rm = T)

#2. Extract row names (id cells)
list_dataframes <- pblapply(list_dataframes, function(x){
  x$ID <- row.names(x); x
})

zero_lenght <- unname(which(sapply(list_dataframes, function(x) dim(x)[2] != 4)))
for(i in zero_lenght){
  list_dataframes[[i]] <- data.frame(x = c(0), y = c(0), layer = c(0), ID = c(0))
}

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
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 2/")
write.csv(distance_dataframe, "distancia_dataframe.csv", row.names = F)


##############################################################################################
##############################################################################################
###                     STRATEGY 2: EFFECTIVE TREATMENTS AND CONTROLS                      ###
###                          CLEANING AND DISTANCE CALCULATION                             ###
###                                    (TERRITORIES)                                       ###
##############################################################################################
##############################################################################################

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

#Create a list of individual polygons per territory. 
polygon_to_list <- function(shape){
  list_polygons <- list()
  for(i in shape@data$ID){
    list_polygons[[i]] <- shape[shape@data$ID == i, ]
  }
  return(list_polygons)
}

list_polygons_buffers <- lapply(buffers_territories, polygon_to_list)
list_polygons_territories <- lapply(territories_proj, polygon_to_list)

#Correct list of polygons (for topo problems)
list_polygons <- list_polygons %>%
  rapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  }, how = "replace")

territories_proj_corrected <- territories_proj %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  })


#Clean polygons
setwd("~")
list_polygons_clean_black_all <- lapply(list_polygons[[1]], clean_treatments, polygon = natural_parks_indigenous_merge,
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

