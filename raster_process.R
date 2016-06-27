library(magrittr)
library(rgdal)

#Open natural parks shapefile
setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
natural_parks <- readOGR(dsn = "WDPA_June2016_COL-shapefile", layer = "WDPA_June2016_COL-shapefile-polygons")

#Remove NP that are out of continental land (Malpelo and Providencia)
natural_parks <- natural_parks[!(natural_parks@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                                                                "Old Providence Mc Bean Lagoon",
                                                                "Malpelo", "Jhonny Cay Regional Park",
                                                                "The Peak Regional Park")), ]

#Calculate distances from cell centrids the the natural parks frontiers
natural_parks_p <- natural_parks %>%
  as("SpatialLines") %>%
  as("SpatialPoints")


#As this process can take a lot of time, we can use cluster computing
beginCluster()

system.time(
  distance_raster <- clusterR(ras[[1]], distanceFromPoints, args = list(xy = natural_parks_p))
)



