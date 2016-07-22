
# Create dataframes for regressions: indigenous and afrocolombian reserves

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
# setwd("Dropbox/BANREP/Deforestacion/")

maps <-"Datos/" 
data <-"Datos/Dataframes/"

########################################################

# Pixels and Reserves data  

########################################################

# Maps (and get correct year)
terr <- list()

terr[[1]] <- readOGR(dsn = paste0(maps,"Resguardos"), layer="Resguardos Indigenas (2015) ") 
terr[[1]]$STATUS_YR <- str_sub(terr[[1]]$RICONSTITU, -4,-1)
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "1-06"] <- 2006 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "1-90"] <- 1990 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "4-03"] <- 2003 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "5-06"] <- 2006 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "7-03"] <- 2003 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "8-87"] <- 1987 
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "9-05"] <- 2005
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "9-85"] <- 1985
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "126"] <- 2013
terr[[1]]$STATUS_YR[terr[[1]]$STATUS_YR == "2136"] <- 2013
terr[[1]]$STATUS_YR <- as.numeric(terr[[1]]$STATUS_YR)

terr[[2]] <- readOGR(dsn = paste0(maps,"Comunidades"), layer="Tierras de Comunidades Negras (2015) ")
terr[[2]]$STATUS_YR <- str_sub(terr[[2]]$RESOLUCION,-4,-1)
terr[[2]]$STATUS_YR[terr[[2]]$STATUS_YR == "dic2"] <- 2003
terr[[2]]$STATUS_YR <- as.numeric(terr[[2]]$STATUS_YR)

# Read distance to Indigenous and black reserves 
dist <- list()

dist[[1]] <- fread(paste0(data,"distance_dataframe_indigenous.csv"))
dist[[2]] <- fread(paste0(data,"distance_dataframe_black.csv"))
  
# Merge and process by terr type

vars <- c("OBJECTID_1","STATUS_YR")

for(i in 1:2){
  
  print(paste0("territorio ",i))
  # Merge  
  terr_b <- terr[[i]][vars]
  dist_b <- dist[[i]]
  dist_b$buffer_id <- as.numeric(dist_b$buffer_id) 
  dist_b <- merge(dist_b, terr_b, by.x="buffer_id", by.y="OBJECTID_1")
  names(dist_b)[names(dist_b) == "layer"] = "dist" 

  # pixel by year (stock)  

  for(y in 2000:2012) {
  
    print(paste0("year ",y))
    eval(parse(text=paste("dist_temp <- dist_b[dist_b$STATUS_YR < ",y,",]", sep="")))
    setorder(dist_temp, ID,-treatment,dist)
    dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
    saveRDS(dist_temp, file =  paste0(data,"dist_terr",i,"_",y,".rds"))
    print(dim(dist_temp))
    
    }  
  }

########################################################

# Panel: 2001-2012

########################################################

for(i in 1:2){

  print(paste0("territorio ",i))
  dist_panel <- list()
  for(y in 2001:2012) {
    print(paste0("year ",y))
    dist_temp <- readRDS(paste0(data,"dist_terr",i,"_",y,".rds"))
    dist_temp <- dist_temp[dist_temp$dist<=20000,]
    dist_temp$year <- y
    dist_panel[[y-2000]] <- dist_temp
  }
  dist_panel <- do.call(rbind, dist_panel)
  
  # Balanced panel: treatment = 0 if park did not exit in year y, and Time-invariant type of park (Use the 2012 park)
  iddat <- expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
  dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
  dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 
  
  desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","buffer_id","dist")]
  names(desig_2012) <- c("ID","buffer_id_2012","dist_2012")
  dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")
  print(dim(dist_panel))
  saveRDS(dist_panel, file =  paste0(data,"dist_panel_terr",i,".rds"))
  }



