library(stringr)
library(raster)
library(magrittr)
library(rgdal)

#Get administrative GIS data
setwd("/Volumes/LaCie/Datos")
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


#Deforestation data (only lossyear)
setwd("/Volumes/LaCie/Deforestacion/Hansen")
files <- list.files() %>%
  str_detect("lossyear")

#Open rasters using raster package and crop to Colombia shape 
loss_year <- lapply(list.files()[files], raster) %>%
  lapply(crop, colombia_municipios) 

#Mosaic images to create a RasterLayer
loss_year$fun <- mean
loss_year$na.rm <- TRUE
system.time(loss_year <- do.call(mosaic, loss_year)) # (15 min)


#If mem is full at this time, it is best to save the mosaic as an individual .tif and to erase all temporary files 
######################################################################################################
 writeRaster(loss_year, filename = "loss_year_mosaic_30m.tif", format = "GTiff", progress = "text")  #
 removeTmpFiles(0.1)                                                                                 #
######################################################################################################

loss_year <- raster("loss_year_mosaic_30m.tif") 

#Layerize values and create a RasterLayer per value (1:13) and sum to a 1km raster
loss_year_1km_year <- layerize(loss_year_1km)

################################## Leonardo's solution ######################################## 
# Another option is to layerize first and then, using the same command, aggreagate the raster #
# by the desired resolution (1 km )                                                           #
###############################################################################################

#Layerize create big files in .grd binary format which does not have any compression. For that reason we change 
# the tmpdir directory to a new one with more space (1 Tb LaCie EHD). 

dir.create("Temp")
rasterOptions(tmpdir = "/Volumes/LaCie/Deforestacion/Hansen/Temp")

system.time(
loss_year_brick <- layerize(loss_year, filename = "loss_year_brick.tif",
                            format = "GTiff",
                            options = "INTERLEAVE=BAND", 
                            progress = "text"))

#Now, we want to create a grid of 1 km2 (using the night light data as reference grid)
fact <- round(dim(loss_year_brick)[1:2] / dim(rasters_lights[[1]])[1:2]) 

agg <- aggregate(loss_year_brick, fact, filename = "loss_year_brick_agg.tf",
                 format = "GTiff",
                 options = "INTERLEAVE = BAND",
                 progress = "text" ) #Proportion of deforestation in the 1 km grid 

agg_mask <- mask(agg, colombia_municipios)
rasters_lights <- setExtent(rasters_lights, agg_mask)

res <- resample(agg_mask, rasters_lights, filename = "loss_year_brick_1km.tif",
                format = "GTiff",
                options = "INTERLEAVE=BAND", 
                progress = "text", overwrite = T)

#Verification that all cells are the same
identical(coordinates(res), coordinates(rasters_lights))




