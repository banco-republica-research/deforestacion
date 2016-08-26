
# Create dataframes for regressions: indigenous and afrocolombian reserves

############################

rm(list=ls())
library(data.table)
library(rgdal)
library(stringr)
library(plyr)
library(dplyr)
library(foreign)
library(rgeos)
library(tidyr)
library(rgdal)

# Leonardo
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
# setwd("Dropbox/BANREP/Deforestacion/")

maps <-"Datos/" 
data <-"Datos/Dataframes/"

########################################################

# Pixels and Reserves data  

########################################################

# original territories for descriptives
indigenous_territories <- readOGR(dsn = paste0(maps,"Resguardos"), layer="Resguardos Indigenas (2015)") 
black_territories <- readOGR(dsn = paste0(maps,"Comunidades"), layer="Tierras de Comunidades Negras (2015)")
colnames(indigenous_territories@data)[8] <- "RESOLUCION"

territories <- 
  list(indigenous_territories, black_territories) %>%
  lapply(spTransform, CRS=CRS("+init=epsg:3857")) %>%
  lapply(., function(x){
  mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", "") , area = gArea(x, byid = T) / 1e6) }) %>%
  lapply(.,as.data.frame)

write.dta(territories[[1]],"Datos/Resguardos/Resguardos.dta")
write.dta(territories[[2]],"Datos/Comunidades/Comunidades.dta")


# Maps (and get correct year)
terr <- list()
terr[[1]] <- readOGR(dsn = paste0(maps,"Resguardos"), layer="Resguardos_clean") 
terr[[2]] <- readOGR(dsn = paste0(maps,"Comunidades"), layer="Comunidades_clean")

for(i in 1:2){
  terr[[i]]$STATUS_YR <- as.numeric(levels(terr[[i]]$year))[terr[[i]]$year] 
  terr[[i]]$ID <- as.numeric(terr[[i]]$OBJECTID_1)-1
}

# Distance: To any frontier, and only effective frontier

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  # Read distance to Indigenous and black reserves 
  dist <- list()
  dist[[1]] <- fread(paste0(data,"Estrategia ",d,"/distance_dataframe_indigenous.csv"))
  dist[[2]] <- fread(paste0(data,"Estrategia ",d,"/distance_dataframe_black.csv"))
  
  # Merge and process by terr type
  
  vars <- c("ID","STATUS_YR")
  
  for(i in 1:2){
    
    print(paste0("territorio ",i))
    # Merge  
    terr_b <- terr[[i]][vars]
    dist_b <- dist[[i]]
    dist_b$buffer_id <- as.numeric(dist_b$buffer_id) 
    dist_b <- merge(dist_b, terr_b, by.x="buffer_id", by.y="ID")
    names(dist_b)[names(dist_b) == "layer"] = "dist" 
    
    # pixel by year (stock)  
    
    for(y in 2000:2012) {
      
      print(paste0("year ",y))
      eval(parse(text=paste("dist_temp <- dist_b[dist_b$STATUS_YR < ",y,",]", sep="")))
      setorder(dist_temp, ID,-treatment,dist)
      dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
      saveRDS(dist_temp, file =  paste0(data,"Estrategia ",d,"/dist_terr",i,"_",y,".rds"))
      print(dim(dist_temp))
      
    }  
  }
}

########################################################

# Panel: 2001-2012

########################################################

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  for(i in 1:2){
    
    print(paste0("territorio ",i))
    dist_panel <- list()
    for(y in 2001:2012) {
      print(paste0("year ",y))
      dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_terr",i,"_",y,".rds"))
      dist_temp <- dist_temp[dist_temp$dist<=20000,]
      dist_temp$year <- y
      dist_panel[[y-2000]] <- dist_temp
    }
    dist_panel <- do.call(rbind, dist_panel)
    
    # Balanced panel: treatment = 0 if park did not exit in year y, and Time-invariant type of park and distance (Use the 2012 park)
    iddat <- expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
    dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
    dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 

    dist_panel$desig_first <- dist_panel$STATUS_YR
    dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2012 
    dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))
    
    desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","buffer_id","dist")]
    names(desig_2012) <- c("ID","buffer_id_2012","dist_2012")
    dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")
    print(dim(dist_panel))
    saveRDS(dist_panel, file =  paste0(data,"Estrategia ",d,"/dist_panel_terr",i,".rds"))

    }
}

