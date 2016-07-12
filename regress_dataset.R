
# Regressions dataframes

############################

rm(list=ls())
library(data.table)
library(rgdal)
library(stringr)
library(plyr)
library(dplyr)


# Leonardo
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 

parks <-"Datos/UNEP/"
data <-"Datos/Dataframes/"

########################################################

# Pixels and parks data  

########################################################

# Parks
parks <- readOGR(dsn = paste0(parks, "WDPA_Modificado"), layer = "TerritoriosResguardados")
vars <- c("ID","DESIG","STATUS_YR","GOV_TYPE")
parks_b <- parks[vars]
summary(parks_b)
desig_year <- table(parks_b$DESIG,parks_b$STATUS_YR)
desig <- table(parks_b$DESIG)

# Distance
dist <- fread(paste0(data,"distancia_dataframe.csv"))
names(dist)[names(dist) == "layer"] = "dist" 
dim(unique(dist,by="ID"))

# Merge  
dist_b <- merge(dist, parks_b, by.x="buffer_id", by.y="ID")
dim(dist_b)

# pixel as any type of park in 2000
dist_2000 <- dist_b[dist_b$STATUS_YR < 2000,]
dist_2000$DESIG2 <- mapvalues(dist_2000$DESIG, levels(dist_2000$DESIG), c(1:16))
dim(dist_2000)
dim(unique(dist_2000, by="ID"))

dist_temp_l <- list()
  
for(i in levels(dist_2000$DESIG2)){
  print(i)
  dist_temp <- dist_2000[dist_2000$DESIG2==i,]
  dist_temp$dup <- duplicated(dist_temp$ID)
  print(table(dist_temp$dup))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  dist_temp_l[[i]] <- dist_temp
  }

lapply(dist_temp_l,dim)




########################################################

# Deforestation

########################################################


defo <- fread(paste0(data,"dataframe_deforestacion.csv"))

