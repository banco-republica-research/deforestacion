library(magrittr)
library(rgdal)

#Open natural parks shapefile
setwd("/Volumes/LaCie/Deforestacion/Parques Naturales")
natural_parks <- readOGR(dsn = "Parques Naturales (2012) - SIGOT", layer="Parques Nacionales Naturales segun Categoria (2012) ") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  
#Remove NP that are out of continental land (Malpelo and Providencia)
natural_parks <- subset(natural_parks, NOM_PARQUE != c("MALPELO", "OLD PROVIDENCE MC BEAN LAGOON ")) #Common subset with "[" don't work when you have NA's in the assing variable

#Calculate distances from cell centrids the the natural parks frontiers
natural_parks_p <- natural_parks %>%
  as("SpatialLines") %>%
    as("SpatialPoints")

system.time(distance_raster <- distanceFromPoints(ras[[1]], natural_parks_p))

  
  
  

