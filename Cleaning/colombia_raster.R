library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(magrittr)


#Get administrative GIS data
setwd("/Volumes/LaCie/Datos/")
colombia_municipios <- 
  readOGR(dsn = "Geografia", layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Remove islands
#Remove municipalities that are out of continental land (Malpelo and Providencia)
colombia_municipios <- 
  colombia_municipios[!(colombia_municipios@data$NOM_MUNICI %in% c("PROVIDENCIA Y SANTA CATALINA (Santa Isabel)",
                                                                   "SAN ANDRÉS", "SANTA CATALINA") | colombia_municipios@data$COD_DEPTO == 88) , ] %>%
  
  gUnaryUnion(.)


#Remove holes
BCp <- slot(colombia_municipios, "polygons")
holes <- lapply(BCp, function(x) sapply(slot(x, "Polygons"), slot, "hole")) 
res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]]) 
IDs <- row.names(colombia_municipios) 
colombia_municipios <- SpatialPolygons(lapply(1:length(res), function(i) Polygons(res[[i]], ID=IDs[i])), proj4string=CRS(proj4string(colombia_municipios))) 
rm(BCp, holes, res, IDs, i)

#Proyect to meters
colombia_municipios_proj <- spTransform(colombia_municipios, CRS=CRS("+init=epsg:3857"))
colombia_municipios <- list(colombia_municipios, colombia_municipios_proj)

#Get nightlight data (only to make a raster with the same specifications)
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


#Buffer (-1 km to clean borders)
colombia_buffer <- colombia_municipios %>%
  spTransform(CRS=CRS("+init=epsg:3857")) %>%
  gBuffer(., width = -1000)


#Rasterize municipios layer
colombia_municipios_r <- rasterize(colombia_municipios, res_mask_natural_parks_buffers) 
colombia_municipios_df <- as.data.frame(colombia_municipios_r, xy = T, na.rm = T)
colombia_municipios_df$ID <- row.names(colombia_municipios_df)


#Rasterize - Julio
colombia_municipios_r2 <- rasterize(colombia_municipios, rasters_lights[[1]],
                                    field = c(colombia_municipios@data$ID_ESPACIA))

setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
write.csv(colombia_municipios_df, "colombia_raster_df", row.names = F)
setwd("~")
writeRaster(colombia_municipios_r2, "colombia_municipios.tif", format = "GTiff")


#Rasters for municipios with codes
colombia_municipios$ID_ESPACIA <- as.numeric(as.character(colombia_municipios$ID_ESPACIA))
colombia_municipios_code_r <- rasterize(colombia_municipios, res[[1]], 
                                         field = c(colombia_municipios$ID_ESPACIA)) %>%
  as.data.frame(xy = T, na.rm = T) %>%
  mutate(ID = row.names(.))

setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
write.csv(colombia_municipios_code_r, "colombia_municipios_code_r", row.names = F)

