
# Create dataframes for regressions

############################

rm(list=ls())
library(data.table)
library(rgdal)
library(rgeos)
library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(foreign)

# Leonardo
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
# setwd("~/Dropbox/BANREP/Deforestacion/")

parks <-"Datos/UNEP/"
data <-"Datos/Dataframes/"

########################################################

# Pixels and parks data  

########################################################

# original parks for descriptives
natural_parks <- readOGR(dsn = paste0(parks, "WDPA_June2016_COL-shapefile"), layer = "WDPA_June2016_COL-shapefile-polygons")
natural_parks <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters
natural_parks$area <- gArea(natural_parks, byid = T) / 1e6
natural_parks <- as.data.frame(natural_parks) %>%
  .[!(.$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                    "Old Providence Mc Bean Lagoon",
                    "Malpelo",
                    "Jhonny Cay Regional Park",
                    "The Peak Regional Park",
                    "Corales De Profundidad",
                    "Los Corales Del Rosario Y De San Bernardo",
                    "Gorgona",
                    "Acandi Playon Y Playona",
                    "Uramba Bahia Malaga")), ]
write.dta(natural_parks,paste0(parks,"natural_parks.dta"))

# Modified parks
parks <- readOGR(dsn = paste0(parks, "WDPA_Modificado"), layer = "WDPA_clean")

vars <- c("ID","DESIG","STATUS_YR","GOV_TYPE")
parks_b <- parks[vars]
summary(parks_b)
desig <- table(parks_b$DESIG)
parks_b$DESIG2 <- mapvalues(parks_b$DESIG, levels(parks_b$DESIG), c(1:15))
table(parks_b$DESIG,parks_b$DESIG2)

# Distance: To any frontier, and only effective frontier

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  dist <- fread(paste0(paste0(data, "Estrategia ",d,"/distancia_dataframe.csv")))
  names(dist)[names(dist) == "layer"] = "dist" 
  
  # Merge 
  dist_b <- merge(dist, parks_b, by.x="buffer_id", by.y="ID")
  #  dist_b$DESIG2 <- mapvalues(dist_b$DESIG, levels(dist_b$DESIG), c(1:15))
  dist_b$year <- as.numeric(dist_b$STATUS_YR)
  dim(dist_b)
  
  # Sólo los creados en 2000 o antes
  for(y in 2001:2001) {
    
    print(paste0("year ",y))
    dist_yl <- list()
    eval(parse(text=paste("dist_y <- dist_b[dist_b$year < ",y,",]", sep="")))
    print(dim(dist_y))
    saveRDS(dist_y, file =  paste0(data,"Estrategia ",d,"/dist_web_",y,".rds"))
    
  }
}



