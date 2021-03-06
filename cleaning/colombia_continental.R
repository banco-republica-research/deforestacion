###############################################################################
########## RASTERIZE COLOMBIA'S FEATURES AND GET COLOMBIAN GEOMS ##############
################ (REMOVE HOLES AND NON-CONTINENTAL LANDS) #####################
###############################################################################

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(magrittr)

# Load functions in R
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/process_rasters.R") 

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

print("LOADING COLOMBIA'S SHAPEFILE")

#Get administrative GIS data
colombia_municipios <- 
  readOGR(dsn = "Geografia" , layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

print("REMOVE NON-CONTINENTAL LAND AND CLEAN GEOMETRY TO SIMPLIFY ANALYSIS
             [REMOVE HOLES AND MAKE A UNARY UNION USING RGEOS]")

#Remove islands
#Remove municipalities that are out of continental land (Malpelo and Providencia)
colombia_municipios_cont <- 
  colombia_municipios[!(colombia_municipios@data$NOM_MUNICI %in% 
                          c("PROVIDENCIA Y SANTA CATALINA (Santa Isabel)",
                            "SAN ANDRÉS", "SANTA CATALINA") | colombia_municipios@data$COD_DEPTO == 88) , ] 

#Proyect to meters
colombia_municipios_proj <- spTransform(colombia_municipios, CRS=CRS("+init=epsg:3857")) %>%
  gUnaryUnion(.) %>% remove_holes(.)

colombia_municipios <- colombia_municipios_cont %>% gUnaryUnion(.) %>% remove_holes(.)
colombia_municipios <- list(colombia_municipios, colombia_municipios_proj)




