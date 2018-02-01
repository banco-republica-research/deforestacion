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
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/process_rasters.R")
source("R/clean_polygons.R")
source("R/calculate_distances.R")
source("cleaning/colombia_continental.R")

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

#Get deforestation raster for reference: deforestation
res <- brick("HansenProcessed/1.4/loss_year_brick_1km.tif")


###############################################################################
#################### READ SHAPEFILES: TERRITORIES DATA  #######################
# 1. As with Natural Parks, here geoms will be stored in lists. In this case, 
# two lists will be created: territories proj in Mercator and territories_proj
# with meters proj. This is set this way to make distance calculations.
###############################################################################

black_territories <- readOGR(dsn = "Comunidades", 
                             layer="Tierras de Comunidades Negras (2015)")

indigenous_territories <- readOGR(dsn = "Resguardos", 
                                  layer="Resguardos Indigenas (2015)") 

# Make some little changes to geom dataframe 

colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Project to meters and remove non-continental parts

territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% 
  lapply(., function(x){
    x@data <- dplyr::mutate(x@data, year = stringr::str_replace_all(
      stringr::str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x[!as.numeric(x@data$year) > 2016, ])
  })  %>%
  #Remove sections of park outside continental Colombia // remove identifiers [BUG]
  lapply(., function(x){
    gIntersection(x, colombia_municipios[[2]], byid = T, drop_lower_td = T)
  }) %>%
  lapply(.,function(x){
    x$ID <- c(1:length(x))
    return(x)
  })
  

###############################################################################
################ CALCULATE BUFFERS AND CONVERT FROM GEOM TO LIST ##############
############################# 50 KM / PROJ AND NON_PROJ #######################
###############################################################################

#Buffers to asses "treatment zones" of 50 km 
buffers_territories <- lapply(territories_proj, gBuffer, byid = T, width = 50000) %>% 
  lapply(., spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Create a list of individual polygons per natural park
list_polygons_territories <- lapply(territories_proj, polygon_to_list)
list_polygons_buffers_territories  <- lapply(buffers_territories, polygon_to_list)


###############################################################################
######### CLEANING BORDERS: NON-VALID BOUNDARIES AND SEA BOUNDARIES ###########
###############################################################################

###############################################################################
################# READ SHAPEFILES: NATURAL PROTECTED AREAS  ###################
# 1. As with territories, here geoms will be stored in lists. In this case, 
# two lists will be created: natural_parks in Mercator and natural_parks_proj
# with meters proj. This is set this way to make distance calculations.
###############################################################################


  #Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
  natural_parks <- readOGR(dsn = paste0("UNEP", "/", "WDPA_June2016_COL-shapefile"), 
                           layer = "WDPA_June2016_COL-shapefile-polygons",
                           stringsAsFactors = F)
  natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters
  
  #Remove NP that are out of continental land and parks after 2016
  natural_parks <- c(natural_parks, natural_parks_proj) %>%
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
       rgeos::gIntersection(y, x)
      #raster::intersect(y, x)
    }, x = . , y = colombia_municipios)


###############################################################################
#################################### NOTE #####################################
# AS WITH NATURAL PARKS, TO CLEANING THE BOUNDARIES WE NEED TO MAKE SOME SP 
# OPERATIONS FIRST. THE MOST DELICATE ONE IS THE INTERSECTION/UNION BETWEEN
# GEOMS WHICH CAN YIELD A LOT OT ERRORS DUE TO GEOM ERRORS LIKE SELF-INTERSECTS
# WE SIMPLIFY GEOMETRIES TO CORRECT THOSE: BUFFER AND SIMPLIFY BEFORE UNIONS
###############################################################################
###############################################################################

territories_hole_free <- lapply(territories_proj, remove_holes)

#Dissolve and merge all geometries to simplify borders (eliminate subgeometries)
territories_dissolve <- lapply(territories_hole_free, function(x){
  unionSpatialPolygons(x, c(1:length(x@polygons))) 
})

territories_union <- lapply(territories_dissolve, gUnaryUnion)

#Clean natural parks from both (create the union between borth territories)
territories_merge <- territories_union %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0.001) %>%
    gSimplify(., tol = 0.001)
  })

#Get union of polygons
natural_parks_indigenous_merge <- rgeos::gUnion(territories_merge[[2]], natural_parks[[2]])
naturaLparks_black_merge <- rgeos::gUnion(territories_merge[[1]], natural_parks[[2]])

###############################################################################
#################################### NOTE #####################################
# TO MAKE DISTANCE CALCULATIONS WE USE POINT DATA INSTEAD OF LINE OR POLYGON
# THIS HAVE SOME ADVANTAGES: IS EASIER TO MERGE POINT GEOMS AND DISTANCE CALCS
# ARE MADE BETWEEN POINTS, SO WE'RE NOT CREATING NOISE IN ANY WAY.
###############################################################################
###############################################################################

black_points <- territories_proj[[1]] %>% to_points()
indigenous_points <- territories_proj[[2]] %>% to_points()
naturalparks_points <- natural_parks[[2]] %>% to_points()
colombia_municipios_p <- colombia_municipios[[2]] %>% to_points()

natural_parks_indigenous_merge_p <- rbind(indigenous_points, naturalparks_points)
naturaLparks_black_merge_p <- rbind(black_points, naturalparks_points)


###############################################################################
###############################################################################


#Mask raster to each shapefile
res_mask_buffers <- lapply(buffers_territories, function(x){
  mask(res[[1]], x)
})

#Identify cells from buffer and territory

cells_territories <- lapply(territories, function(x) cellFromPolygon(res[[1]], x))

cells_territories_buffers <- lapply(buffers_territories, function(x){
  cellFromPolygon(res[[1]], x)
})

list_polygons_buffers <- lapply(buffers_territories, polygon_to_list)
list_polygons_territories <- lapply(territories_proj, polygon_to_list)

#Correct list of polygons (for topo problems)
list_polygons_territories <- list_polygons_territories %>%
  rapply(function(x){
    geom <- gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001) 
        return(x)
  }, how = "replace")

territories_proj_corrected <- territories_proj %>%
  lapply(function(x){
    gBuffer(x, byid = T, width = 0) #%>%
      #gSimplify(., tol = 0.001)
    })

natural_parks_corrected <- natural_parks[[2]] %>%
  gBuffer(., byid = T, width = 0) %>%
  gSimplify(., tol = 0.001)


#Clean polygons
list_polygons_clean_black_all <- lapply(list_polygons_territories[[1]], 
                                        clean_treatments, 
                                        polygon = natural_parks_indigenous_merge,
                                        points_sp = natural_parks_indigenous_merge_p, 
                                        points_border = colombia_municipios_p,
                                        shape = territories_proj_corrected[[1]])
saveRDS(list_polygons_clean_black_all, "list_polygons_clean_black_all_2016.rds")

list_polygons_clean_indigenous_all <- lapply(list_polygons_territories[[2]], clean_treatments, 
                                             polygon = naturaLparks_black_merge,
                                             points_sp = naturaLparks_black_merge_p, 
                                             points_border = colombia_municipios_p,
                                             shape = territories_proj_corrected[[2]])
saveRDS(list_polygons_clean_indigenous_all, "list_polygons_clean_indigenous_all_2016.rds")


#Does it work? 
plot(list_polygons_territories[[2]][[708]])
plot(naturaLparks_black_merge, add = T, border = "red")
plot(list_polygons_clean_indigenous_all[[708]], add = T, col = "blue", pch = 19)

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


###############################################################################
################# CALCLATE DISTANCES TO EFFECTIVE BORDERS #####################
###############################################################################

###############################################################################
################################## NOTE #######################################
# THIS FOLLOWING CODE CALCULATES DISTANCE TO CLEAN BORDERS FOR EACH BLACK AND
# INDIGENOUS POLYGON. FOR THIS, WE USE THE FUNCTION CALCULATE DISTANCES PARALLEL
# WHICH ALLOW US TO CALCULATE THESE DISTANCES USING MULTIPLE CORES. WE USE A
# CROPPED RASTER (res_mask_buffers) TO SIMPLIFY THE CALCULATION.
###############################################################################
###############################################################################

beginCluster()
system.time(distances_black <- mapply(calculate_distances_parallel_territories,
                                      buffer = list_polygons_buffers[[1]], 
                                      points = list_polygon_clean_proj[[1]]))
endCluster()
saveRDS(mask_distance, "rds_data/distances_black_2016.rds")

beginCluster()
system.time(distances_indigenous <- mapply(calculate_distances_parallel_territories,
                                           buffer = list_polygons_buffers[[2]], 
                                           points = list_polygon_clean_proj[[2]]))
endCluster()
saveRDS(mask_distance, "rds_data/distances_indigenous_2016.rds")

###############################################################################
#################### EXTRACT DISTANCE VALUES FROM RASTERS #####################
###############################################################################

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

###############################################################################
################################## NOTE #######################################
# SINCE SOME POLYGONS ARE MISSING (IN THE CLEANING PROCESS NO EFFECTIVE BORDER
# IS FOUND). WE NEED TO ACCOUNT THEM IN THE LIST AND CREATE A FAKE DATA FRAME 
# TO IDENTIFY THEM.
###############################################################################

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
  write.csv(list[[i]], file = str_c("Dataframes/Estrategia 2/distance_dataframe_2016_", names(list)[[i]], ".csv") , row.names = FALSE)
})
