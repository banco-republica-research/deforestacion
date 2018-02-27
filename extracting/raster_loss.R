###############################################################################
######################### EXTRACT DATA FROM DEFORESTATION #####################
##### PROCESS HANSEN DATA: CHANGE RESOLUTION AND PREPARE TO EXTRACT DATA ######
###############################################################################

rm(list=ls())
library(stringr)
library(raster)
library(magrittr)
library(rgdal)

#Load functions in R
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/process_rasters.R") 

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))


#Get administrative GIS data
colombia_municipios <- 
  readOGR(dsn = paste0("Geografia"), layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
list_raster <- list.files(paste0(paste0(data, "NOAA2/TIFF/")), full.names = TRUE) %>%
  lapply(raster)
rasters_extent <- extent(list_raster[[1]]) #We need to put all rasters into the same extent (all have the same resolution)
rasters_lights <- processing_rasters(list_raster, rasters_extent, colombia_municipios)

#Deforestation data (only lossyear)
files <- list.files(paste0("hansen_raw"), full.names = TRUE) %>%
  str_detect("lossyear")

#Open rasters using raster package and crop to Colombia shape 
loss_year <- lapply(list.files(paste0("hansen_raw"), full.names = TRUE)[files], raster) %>%
  lapply(crop, colombia_municipios) 

#Mosaic images to create a RasterLayer
loss_year$fun <- mean
loss_year$na.rm <- TRUE
system.time(loss_year <- do.call(mosaic, loss_year)) # (15 min)


#If mem is full at this time, it is best to save the mosaic as an individual .tif and to erase all temporary files 
######################################################################################################
writeRaster(loss_year, filename = paste0("HansenProcessed/1.4/loss_year_mosaic_30m.tif"), 
            format = "GTiff", 
            progress = "text") 
removeTmpFiles(0.1)                                                                                 
######################################################################################################

loss_year <- raster(paste0(data, "HansenProcessed/1.4/loss_year_mosaic_30m.tif")) 

################################## Leonardo's solution ######################################## 
# Another option is to layerize first and then, using the same command, aggreagate the raster #
# by the desired resolution (1 km )                                                           #
###############################################################################################

# Layerize create big files in .grd binary format which does not have any compression. All the processing
# is done in the swap memory, hence be cautious of free disk space . 

system.time(
loss_year_brick <- layerize(loss_year, filename = paste0("HansenProcessed/1.4/loss_year_brick.tif"),
                            format = "GTiff",
                            options = "INTERLEAVE=BAND", 
                            progress = "text"))

#Now, we want to create a grid of 1 km2 (using the night light data as reference grid)
fact <- round(dim(loss_year_brick)[1:2] / dim(rasters_lights[[1]])[1:2]) 

agg <- aggregate(loss_year_brick, fact, filename = paste0("HansenProcessed/1.4/loss_year_brick_agg.tif"),
                 format = "GTiff",
                 options = "INTERLEAVE = BAND",
                 progress = "text" ) #Proportion of deforestation in the 1 km grid 

agg_mask <- mask(agg, colombia_municipios)
rasters_lights <- setExtent(rasters_lights, agg_mask)

res <- resample(agg_mask, rasters_lights, filename = paste0("HansenProcessed/1.4/loss_year_brick_1km.tif"),
                format = "GTiff",
                options = "INTERLEAVE=BAND", 
                progress = "text", overwrite = T)

#Verification that all cells are the same
identical(coordinates(res), coordinates(rasters_lights))

#Extract data to dataframe (csv)
res_df <- as.data.frame(res, na.rm = T, xy = T)
res_df$ID <- row.names(res_df)
write.csv(res_df, "Dataframes/dataframe_deforestacion.csv")





