###############################################################################
####################### EXTRACT DATA FROM VARIOUS RASTERS #####################
############################ (ESTIMATION COVARIATES) ##########################
###############################################################################
rm(list=ls())
library(raster)
library(magrittr)
library(rgdal)
library(rgeos)
library(plyr)
library(dplyr)
library(stringr)

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
######################## NIGHT LIGHTS CLUMPS (FROM NOAA) ######################
###############################################################################

# Open nightlight .tif files as a raster and create a multiband file
list_raster <- list.files(paste0("NOAA2/TIFF/"), full.names = TRUE) %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, colombia_municipios[[1]])
writeRaster(rasters_lights[[1]], paste0("NOAA2/", "light_brick_colombia.tif"), overwrite = TRUE)

#Clumps to identify cities (queen) for 2005
clump_lights <- clump(rasters_lights[[13]], directions = 8) %>%
  resample(., res[[1]])
clump_lights[clump_lights > 1] <- 1

p1 <- rasterToPolygons(clump_lights, dissolve = T)

#Set a bigger zone of light influence (buffer: 5 and 10 k's)

p1_buffer_5km <-  rasterToPolygons(clump_lights, dissolve = T) %>%
  spTransform(CRS=CRS("+init=epsg:3857")) %>%
  gBuffer(width = 5000) %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


p1_buffer_10km <-  rasterToPolygons(clump_lights, dissolve = T) %>%
  spTransform(CRS=CRS("+init=epsg:3857")) %>%
  gBuffer(width = 10000) %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


clumps_buffer_5k <- cellFromPolygon(res[[1]], p1_buffer_5km)
clumps_buffer_10k <- cellFromPolygon(res[[1]], p1_buffer_10km)



#Clumps to polygons
p1 <- rasterToPolygons(clump_lights, dissolve = T)
writeOGR(p1, paste0("Clumps", "/", "polygon_clump_layer_2000.shp"), 
         layer = "clumps", 
         driver = "ESRI Shapefile", 
         overwrite_layer = T)


###############################################################################
######################### ELEVATION (FROM DEM 2010) ###########################
###############################################################################

#Process data
elevation <- raster(paste0("Rasters", "/", "altura_tile_30arc.tif")) %>%
  crop(colombia_municipios[[1]]) %>%
  mask(colombia_municipios[[1]]) %>%
  resample(., res[[1]])

# Calcuate slope and aspect
slope <- terrain(elevation, opt = "slope")
roughness  <- terrain(elevation, opt = "roughness")
tri <- terrain(elevation, opt = "TRI")


###############################################################################
###################### CLIMATE VARIABLES: PRECIPITATION  ######################
########################### (FROM WORLDCLIM PROJECT) ##########################
###############################################################################

#Get climate 
list_files <- list.files(paste0("Rasters", "/", "wc2"), full.names = T) %>%
  str_detect("prec")

prec <- lapply(list.files()[list_files], raster) %>%
  brick() %>%
  lapply(crop, colombia_municipios[[1]]) %>%
  lapply(mask, colombia_municipios[[1]]) %>%
  resample(res[[1]], filename = paste0("Rasters", "/", "prec_1km.tif"),
           format = "GTiff",
           options = "INTERLEAVE=BAND", 
           progress = "text", overwrite = T)


###############################################################################
########################### SOIL QUALITY (FROM FAO) ###########################
###############################################################################

list_files <- list.files(paste0("Rasters", "/", "soil_quality"), full.names = T)

sq <- lapply(list_files, raster) %>%
  brick() %>%
  crop(colombia_municipios) %>%
  mask(colombia_municipios) %>%
  resample(res[[1]], filename = paste0("Rasters", "/", "soil_quality", "/", "sq_1km.tif"),
           format = "GTiff",
           options = "INTERLEAVE=BAND", 
           progress = "text", overwrite = T)


###############################################################################
############################ ROADS (FROM DANE) ################################
###############################################################################

#Roads
roads <- readOGR(dsn = "Roads", layer="VIAS") %>%
  # spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) to mercator
  spTransform(CRS=CRS("+init=epsg:3857")) 

buffer_roads <- gBuffer(roads, 5000, byid = TRUE, id = row.names(roads)) %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cells_roads <- cellFromPolygon(res[[1]], buffer_roads)

cells_roads <- unlist(cells_roads)
write.csv(cells_roads, paste0("Dataframes", "/", "roads.csv"))


#Extract data to data.frame
dataframes_extract <- list(elevation, slope, roughness, tri, clump_lights, prec, sq) %>%
  lapply(as.data.frame, na.rm  = T) %>%
  lapply(function(x){
    x$ID <- row.names(x); x
  })

#Merge
merge_rasters_dataframes <- Reduce(function(...) merge(..., by="ID", all = T), dataframes_extract) %>%
  mutate(roads = ifelse(ID %in% unlist(cells_roads), 1, 0)) %>%
  mutate(clumps_5k = ifelse(ID %in% unlist(clumps_buffer_5k), 1 ,0)) %>%
  mutate(clumps_10k = ifelse(ID %in% unlist(clumps_buffer_10k), 1, 0)) %>%
  mutate(clumps_1 = ifelse(is.na(clumps), 0, 1)) %>%
  mutate(prec = select(., starts_with("prec")) %>% rowMeans(na.rm = TRUE))

#Export .csv with clumps and ID
write.csv(merge_rasters_dataframes, paste0("Dataframes", "/", "geographic_covariates_new.csv"))





