##############################################################################################
##############################################################################################
###                     STRATEGY 2: EFFECTIVE TREATMENTS AND CONTROLS                      ###
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
setwd(Sys.getenv("DATA_FOLDER"))

#Get deforestation raster for reference: deforestation
res <- brick("HansenProcessed/1.4/loss_year_brick_1km.tif")

###############################################################################
######### READ SHAPEFILES: NATURAL PROTECTED AREAS AND ARRANGE DATA ###########
###############################################################################

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
natural_parks <- readOGR(dsn = paste0("UNEP", "/", "WDPA_June2016_COL-shapefile"), 
                                      layer = "WDPA_June2016_COL-shapefile-polygons",
                         stringsAsFactors = F)
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Remove NP that are out of continental land and parks after 2016
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
#     #rgeos::gIntersection(y, x)
#     }, x = . , y = colombia_municipios)


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
################ CALCULATE BUFFERS AND CONVERT FROM GEOM TO LIST ##############
############################# 50 KM / PROJ AND NON_PROJ #######################
###############################################################################

#Buffers to asssess "treatment zones" of 50 km 
buffers_natural_parks_proj <- gBuffer(natural_parks[[2]], byid = T, width = 50000)
buffers_natural_parks <- spTransform(buffers_natural_parks_proj, 
                                     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Create a list of individual polygons per natural park
list_polygons <- polygon_to_list(natural_parks[[2]])
list_polygons_buffers  <- polygon_to_list(buffers_natural_parks)


mapply(function(x,y){
  if(class(y)[1] == "SpatialPointsDataFrame"){
    value = gTouches(x, y)
  } else
    value = 0
  return(value)
} , x = list_polygons, y = list_polygons_clean_all, SIMPLIFY = TRUE)



######################## TEST: BUFFERS = POLYGONS ########################
id1 <-list()
for(i in c(1:length(natural_parks[[2]]@polygons))){
  
  id1[[i]] <- slot(natural_parks[[2]]@polygons[[i]], "ID")
}

id2 <-list()
for(i in c(1:length(buffers_natural_parks@polygons))){
  
  id2[[i]] <- slot(buffers_natural_parks@polygons[[i]], "ID")
}
identical(id1, id2)
rm(id1, id2, i)
################################ END OF TEST ##############################


###############################################################################
######### CLEANING BORDERS: NON-VALID BOUNDARIES AND SEA BOUNDARIES ###########
###############################################################################

###############################################################################
#################### READ SHAPEFILES: TERRITORIES DATA  #######################
# 1. As with Natural Parks, here geoms will be stored in lists. In this case, 
# two lists will be created: territories proj in Mercator and territories_proj
# with meters proj. This is set this way to make distance calculations.
###############################################################################

black_territories <- readOGR(dsn = paste0("Comunidades/"), 
                             layer="Tierras de Comunidades Negras (2015)")

indigenous_territories <- readOGR(dsn = paste0("Resguardos/"), 
                                  layer="Resguardos Indigenas (2015)") 

# Make some little changes to geom dataframe 

colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Project to meters and remove non-continental parts

territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% 
  lapply(., function(x){
    x@data <- mutate(x@data, year = str_replace_all(
      str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""))
    return(x[!as.numeric(x@data$year) > 2016, ])
  })  %>%
  #Remove sections of park outside continental Colombia
  lapply(., function(x){
    gIntersection(x, colombia_municipios[[2]], byid = T, drop_lower_td = T)
  }) %>%
  lapply(.,function(x){
    x$ID <- c(1:length(x))
    return(x)
  })

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


###############################################################################
################################### NOTES #####################################
# 1. GEOM CALCULATIONS CAN BE TRICKY, SPECIALLY IF GEOMS HAVE SP ERRORS LIKE 
# NON-CLOSED CIRLES OR INVALID INTERSECTIONS OF ANY TYPE. TO REDUCE ERRORS IN 
# THIS PROCESSESS, WE REMOVE HOLES FROM GEOMS (LAKES, ETC.) USING THE remove_holes
# FUNCTION IN ./R
# 2. AFTER REMOVING HOLES FROM GEOMS, WE DISSOLVE POLYGONS AND CREATE UNIONS 
# (UnaryUnions) TO SIMPLIFY GEOMS AND GET THEM READY TO INTERSECT: REMOVE SUB
# GEOMETRIES AND THEN UNARY UNION.
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
    gBuffer(x, byid = T, width = 0) %>%
      gSimplify(., tol = 0.001)
  })

natural_parks_corrected <- natural_parks[[2]] %>%
  gBuffer(., byid = T, width = 0) %>%
  gSimplify(., tol = 0.001)

list_polygons <- lapply(list_polygons, gBuffer, width = 0, byid = T)

# Use simplify geometries to create a new geom made of the intersection
# of the territories
territories_merge <- raster::union(territories_merge[[1]], territories_merge[[2]])
territories_merge_p <- rbind(black_points, indigenous_points)

###############################################################################
###################### CLEAN ALL NATURAL PARK BORDERS #########################
###############################################################################

list_polygons_clean_all <- lapply(list_polygons, 
                                  clean_treatments, 
                                  polygon = territories_merge,
                                  points_sp = territories_merge_p, 
                                  points_border = colombia_municipios_p,
                                  shape = natural_parks_corrected)



# Save to .rds
saveRDS(list_polygons_clean_all, "rds_data/list_polygons_clean_all_2016.rds")


####### VISUAL VERIFICATION OF THE CLEANING PROCESS - PNN CHIRIBIQUETE ########
chiribiquete_clean <- clean_treatments(list_polygons[[8]], 
                                       polygon = territories_merge,
                                       points_sp = territories_merge_p, 
                                       points_border = colombia_municipios_p,
                                       shape = natural_parks_corrected)
plot(list_polygons[[8]])
plot(chiribiquete_clean, add = T, col = "red")
plot(territories_merge_p, add = T, col = "blue")
###############################################################################

###############################################################################
#################################### NOTE #####################################
# 1. ONCE THE CLEANNING PROCESS HAS ENDED, WE PROJECT THE CLEANED GEOM TO 
# METERS AND IDENTIFY RASTER CELLS THAT ARE INSIDE EACH OF THE TERRITORIES 
# (TREATMENT CELLS) OR INSIDE THE BUFFER (CONTROL CELLS)
###############################################################################
###############################################################################

#Reproject list to raster projection (WGS84)
list_polygons_clean_all_proj <- lapply(list_polygons_clean_all, function(x){
  if(typeof(x) == "S4" & length(x) > 0){
    sp <- spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(sp)
  } else { 
  return(0)
  }
})

#Identify cells inside national parks and buffers and their identifier
cells_naturalparks <- cellFromPolygon(res[[1]], natural_parks[[1]])
cells_naturalparks_buffers <- cellFromPolygon(res[[1]], buffers_natural_parks)
cells <- mapply(function(x, y){ #Remove cells from natural park polygons and list only the buffer pixels
  x[! x %in% y]
}, x = cells_naturalparks_buffers, y = cells_naturalparks)

#Mask raster to values indices buffers
res_mask_natural_parks_buffers <- mask(res[[1]], buffers_natural_parks)

###############################################################################
############################ DISTANCE CALCULATIONS ############################
###############################################################################

###############################################################################
#################################### NOTE #####################################
# 1. THE FOLLOWING FUNCTIONS WILL CALCULATE THE DISTANCE OF EACH BUFFER CELL TO 
# ITS CLEAN  BOUNDARY (OUTCOME OF clean_polygon). 
# 2. BE AWARE THAT THIS PROCESS IS PARALIZED USING SNOW, SO THE MORE CORES, THE
# BETTER. THE PROCESS CAN TAKE UP TO 15 MINS USING 3 CORES.
###############################################################################
###############################################################################

beginCluster()
system.time(mask_distance <- mapply(calculate_distances_parallel,
                           buffer = list_polygons_buffers, 
                           points = list_polygons_clean_all_proj))
endCluster()

###############################################################################
####################### EXTRACT DISTANCE CALCULATIONS #########################
###############################################################################

#Extract distance as data frame per buffer (list element)
list_dataframes <- pblapply(mask_distance, as.data.frame, xy = T, na.rm = T)

#Extract row names (id cells)
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
write.csv(distance_dataframe, 
          paste0(data, "Dataframes", "/", "Estrategia 2", "/", "distancia_dataframe_2016.csv"), row.names = F)
