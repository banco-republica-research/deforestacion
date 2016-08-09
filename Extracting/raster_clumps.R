#Get administrative GIS data
setwd("/Volumes/LaCie/Datos/")
colombia_municipios <- 
  readOGR(dsn = "Geografia", layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


#Remove islands
#Remove municipalities that are out of continental land (Malpelo and Providencia)
colombia_municipios <- 
  colombia_municipios[!(colombia_municipios@data$NOM_MUNICI %in% c("PROVIDENCIA Y SANTA CATALINA (Santa Isabel)",
                           "SAN ANDRÉS", "SANTA CATALINA") | colombia_municipios@data$COD_DEPTO == 88) , ]

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
clump_lights <- clump(rasters_lights[[22]], directions = 8) %>%
  resample(., res[[1]])
clump_lights_df <- as.data.frame(clump_lights, xy = T, na.rm = T)
clump_lights_df$ID <- row.names(clump_lights_df)
clump_lights_df$clumps <- 1 #Remove clump identifier (because yes ;] )


#Clumps to polygons
p1 <- rasterToPolygons(clump_lights, dissolve = T)
setwd("~/Dropbox/BANREP/Deforestacion/Datos")
dir.create("Clumps", showWarnings = T)
writeOGR(p1, "Clumps/polygon_clump_layer.shp",layer =  "clumps", driver = "ESRI Shapefile")

#Export .csv with clumps and ID
write.csv(clump_lights_df, "Clumps/clump_id_dataframe.csv")



