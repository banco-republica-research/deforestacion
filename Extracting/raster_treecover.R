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


#Deforestation data (treecover)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Hansen/")
files <- list.files() %>%
  str_detect("treecover")

treecover <- lapply(list.files()[files], raster) %>%
  lapply(crop, colombia_municipios)

#Mosaic images to create a RasterLayer
treecover$fun <- mean
treecover$na.rm <- TRUE
system.time(treecover <- do.call(mosaic, treecover)) # (15 min)

writeRaster(treecover, filename = "treecover_mosaic_30m.tif", format = "GTiff", progress = "text")
removeTmpFiles(0.1)

#Convert raster to 1 km resolution (aggregate)

fact <- round(dim(treecover)[1:2] / dim(rasters_lights[[1]])[1:2]) 

setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
agg <- aggregate(treecover, fact, filename = "treecover_agg.tif",
                 format = "GTiff",
                 options = "INTERLEAVE = BAND",
                 progress = "text",
                 overwrite = TRUE) #Proportion of deforestation in the 1 km grid 

agg_mask <- mask(agg, colombia_municipios)
rasters_lights <- setExtent(rasters_lights, agg_mask)

res <- resample(agg_mask, rasters_lights, filename = "treecover_1km.tif",
                format = "GTiff",
                options = "INTERLEAVE=BAND", 
                progress = "text", overwrite = T)

#Verification that all cells are the same
identical(coordinates(res), coordinates(rasters_lights))

#Extract data
treecover_csv <- as.data.frame(res, xy = TRUE, na.rm = TRUE)
treecover_csv$ID <- row.names(treecover_csv)
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/")
write.csv(treecover_csv, "treecover_2000.csv")

