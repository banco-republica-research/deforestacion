library(raster)
library(magrittr)
library(rgdal)
library(rgeos)
library(plyr)
library(dplyr)
library(stringr)

#Get deforestation raster for reference 
setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
res <- brick("loss_year_brick_1km.tif")

#Get administrative GIS data
setwd("/Volumes/LaCie/Datos/")
colombia_municipios <- 
  readOGR(dsn = "Geografia", layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Get nightlight data
processing_rasters <- function(layer.list, ext, shape){
  layer.list %>%
    lapply(setExtent, ext) %>%
    lapply(crop, shape) %>%
    stack() %>% 
    mask(shape)
}

# Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
setwd("~")
setwd("/Volumes/LaCie/NOAA2/TIFF/")

list_raster <- list.files() %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, colombia_municipios) 

#Clumps to identify cities (queen) for 2005
clump_lights <- clump(rasters_lights[[13]], directions = 8) %>%
  resample(., res[[1]])
clump_lights[clump_lights > 1] <- 1

#Clumps to polygons
p1 <- rasterToPolygons(clump_lights, dissolve = T)
setwd("~/Dropbox/BANREP/Deforestacion/Datos")
writeOGR(p1, "Clumps/polygon_clump_layer_2000.shp",layer = "clumps", driver = "ESRI Shapefile", overwrite_layer = T)


#Process data
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Rasters/")
elevation <- raster("altura_tile_30arc.tif") %>%
  crop(colombia_municipios) %>%
  mask(colombia_municipios) %>%
  resample(., res[[1]])

# 6. Slope and aspects
slope <- terrain(elevation, opt = "slope")
roughness  <- terrain(elevation, opt = "roughness")
tri <- terrain(elevation, opt = "TRI")

#Get climate 
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Rasters/wc2/")
list_files <- list.files() %>%
  str_detect("prec")

prec <- lapply(list.files()[list_files], raster) %>%
  brick() %>%
  lapply(crop, colombia_municipios) %>%
  lapply(mask, colombia_municipios) %>%
  resample(res[[1]], filename = "prec_1km.tif",
           format = "GTiff",
           options = "INTERLEAVE=BAND", 
           progress = "text", overwrite = T)

#Soil quality 
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Rasters/soil_quality/")
list_files <- list.files()

sq <- lapply(list_files, raster) %>%
  brick() %>%
  crop(colombia_municipios) %>%
  mask(colombia_municipios) %>%
  resample(res[[1]], filename = "sq_1km.tif",
           format = "GTiff",
           options = "INTERLEAVE=BAND", 
           progress = "text", overwrite = T)

#Roads
setwd("~/Dropbox/BANREP/Deforestacion/Datos/")
roads <- readOGR(dsn = "Roads", layer="VIAS") %>%
  # spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) to mercator
  spTransform(CRS=CRS("+init=epsg:3857")) 

buffer_roads <- gBuffer(roads, 5000, byid = TRUE, id = row.names(roads)) %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cells_roads <- cellFromPolygon(res[[1]], buffer_roads)

cells_roads <- unlist(cells_roads)
write.csv(cells_roads, "roads.csv")


#Extract data to data.frame
dataframes_extract <- list(elevation, slope, roughness, tri, clump_lights, prec, sq) %>%
  lapply(as.data.frame, na.rm  = T) %>%
  lapply(function(x){
    x$ID <- row.names(x); x
  })

#Merge
merge_rasters_dataframes <- Reduce(function(...) merge(..., by="ID", all = T), dataframes_extract) %>%
  mutate(roads = ifelse(ID %in% unlist(cells_roads), 1, 0)) %>%
  mutate(clumps_1 = ifelse(is.na(clumps), 0, 1)) %>%
  mutate(prec = select(., starts_with("prec")) %>% rowMeans(na.rm = TRUE))

#Export .csv with clumps and ID
setwd("~/Dropbox/BANREP/Deforestacion/Datos/")
write.csv(merge_rasters_dataframes, "geographic_covariates.csv")





