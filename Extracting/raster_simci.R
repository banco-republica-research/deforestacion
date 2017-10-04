library(stringr)
library(raster)
library(magrittr)
library(rgdal)
library(foreign)

#Load functions in R
source("R/process_rasters.R") 

data <- "Deforestacion/Datos/"

setwd("~/Dropbox/BANREP/")

colombia_municipios <- 
  readOGR(dsn = paste0(data, "Geografia"), layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
list_raster <- list.files(paste0(paste0(data, "NOAA2/TIFF/")), full.names = TRUE) %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, colombia_municipios)
writeRaster(rasters_lights[[1]], paste0(data, "NOAA2/", "light_brick_colombia.tif"), overwrite = TRUE)
  
# "Coca_01_16_grillas1k", 
# "EVOA_MTPL_1k_18N"

simci_data <- lapply(c("Coca_01_16_grillas1k", "EVOA_MTPL_1k_18N"), function(x){
  st_read(dsn = paste0(data, "SIMCI"), layer = x) %>%
    as(., "Spatial") %>%
    spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
})

r_ <- raster::rasterize(x = r, 
                        y = rasters_lights,
                        field = names(r)
                        )
  