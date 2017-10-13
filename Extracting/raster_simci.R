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

  
####################################################################
####################### RASTERIZE SHAPEFILES #######################
####################################################################

# THIS PROCESS WAS MADE IN PYTHON FOR EFFICIENCY REASONS. PYTHON IS
# FASTER TO OPEN GRID POLYGON DATA AND MADE A COMPARABLE JOB TO R 
# WHEN CREATING RASTERS. THIS CODE CAN BE USE IN CASE YOU WANT TO HAVE
# ALL THE DATA CREATION PIPELINE IN R.

# Open shapefiles and project to a common reference
simci_data <- lapply(c("Coca_01_16_grillas1k", "EVOA_MTPL_1k_18N"), function(x){
  readOGR(dsn = paste0(data, "SIMCI"), layer = x) %>%
    spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
})

# Example of rasterize
r_ <- raster::rasterize(x = simci_data[[1]], 
                        y = rasters_lights[[1]],
                        field = "COCA_C_11",
                        filename = "coca_COCA_C_11.tif",
                        overwrite = T
                        )
######################################################################


res <- brick(paste0(data, "HansenProcessed", "/", "loss_year_brick_1km.tif"))
treecover <- brick(paste0(data, "HansenProcessed", "/", "treecover_1km.tif"))


#Open rasterized rasters from Python (raster_simci.py)
file_names <- c("coca", "illegal_mining")

simci_rasters <- lapply(file_names, function(x){
  #Open and process rasters (crop and set extent)
  proc_raster <- list.files(paste0(data, "SIMCI"), full.names = TRUE) %>%
    .[str_detect(., x)] %>%
    lapply(raster) %>%
    processing_rasters(., extent(res), colombia_municipios)
  
  #Resample to deforestation data (make all pixels comparable - last resort after crop and extent)
  raster_resampled <- resample(proc_raster, res, filename = paste0(data, "SIMCI", "/", x, "_1km_brick.tif"),
                               format = "GTiff",
                               options = "INTERLEAVE=BAND", 
                               progress = "text", overwrite = T)
  
  #Extract data
  raster_plain_csv <- as.data.frame(raster_resampled, xy = TRUE, na.rm = TRUE)
  raster_plain_csv$ID <- row.names(raster_plain_csv)
  write.csv(raster_plain_csv, paste0(data, "Dataframes", "/", x, "simci.csv"))
})

identical(coordinates(simci_rasters[[1]]), coordinates(res[[1]]))
