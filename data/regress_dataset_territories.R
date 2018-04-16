##############################################################################################
##############################################################################################
###                            CREATE DATAFRAMES FOR REGRESSION                            ###
###     THIS CODE CREATES A YEAR DATAFRAME FOR EACH TERRITORY ACCORDING TO THE NEARER      ###
###     PIXEL TO THE DISCONTINUITY BORDER CLEANED WITH THE PREVIOUS CODE                   ###
##############################################################################################
##############################################################################################

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

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

# Path viejo
setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Deforestacion/Datos/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/Datos/")

###############################################################################
####### READ SHAPEFILES: INDIGENOUS AND BLACK AREAS AND ARRANGE DATA ##########
###############################################################################

black_territories <- readOGR(dsn = "Comunidades", 
                             layer="Tierras de Comunidades Negras (2015)")

indigenous_territories <- readOGR(dsn = "Resguardos", 
                                  layer="Resguardos Indigenas (2015)") 

# Make some little changes to geom dataframe 

colnames(indigenous_territories@data)[8] <- "RESOLUCION"
territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Project to meters and remove non-continental parts and create ID and year variables 

territories_proj <- lapply(territories, spTransform, CRS=CRS("+init=epsg:3857")) %>% 
  lapply(., function(x){
    x@data <- dplyr::mutate(x@data, year = stringr::str_replace_all(
      stringr::str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", "")) %>%
      mutate(., ID = c(1:dim(.)[[1]])) %>%
      mutate(., STATUS_YR = as.numeric(as.character(year))) 
    return(x[!as.numeric(x@data$year) > 2016, ])
  }) 

#Export shapefile metadadata to .dta
write.dta(territories_proj[[1]]@data,"Resguardos/Resguardos.dta")
write.dta(territories_proj[[2]]@data,"Comunidades/Comunidades.dta")


###############################################################################
######## LOOP BETWEEN YEARS AND TERRITORY TYPES TO CREATE DATA FRAMES #########
###############################################################################
# THIS LOOP CREATES DATAFRAMES (SAVED AS RDS R FILES) FOR EACH YEAR AND TYPE  #
# OF PROTECTED AREA (INDIGENOUS, BLACK), THIS CODE READS THE DISTANCE FILE    #
# AND SELECTS FOR EACH YEAR AND TYPE THE NEARER EFFECTIVE FRONTIER. THIS      #
# WAY ALLOW US TO CORRECT IDENTIFY THE PIXEL AND ITS FEATURES                 #
###############################################################################

# Distance: To any frontier, and only effective frontier

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  # Read distance to Indigenous and black reserves 
  dist <- list()
  dist[[1]] <- fread(paste0("Dataframes/","Estrategia ",d,"/distance_dataframe_black.csv"))
  dist[[2]] <- fread(paste0("Dataframes/","Estrategia ",d,"/distance_dataframe_indigenous.csv"))
  
  # Merge and process by terr type
  
  vars <- c("ID","STATUS_YR")
  
  for(i in 1:2){
    
    print(paste0("territorio ",i))
    # Merge  
    terr_b <- territories_proj[[i]][vars]
    dist_b <- dist[[i]]
    dist_b$buffer_id <- as.numeric(dist_b$buffer_id) 
    dist_b <- merge(dist_b, terr_b, by.x="buffer_id", by.y="ID")
    names(dist_b)[names(dist_b) == "layer"] = "dist" 
    
    # pixel by year (stock)  
    
    for(y in 2000:2016) {
      
      print(paste0("year ",y))
      eval(parse(text=paste("dist_temp <- dist_b[dist_b$STATUS_YR < ",y,",]", sep="")))
      setorder(dist_temp, ID,-treatment,dist)
      dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
      saveRDS(dist_temp, file =  paste0("Dataframes/","Estrategia ",d,"/dist_terr",i,"_",y,".rds"))
      print(dim(dist_temp))
      
    }  
  }
}

###############################################################################
########### LOOP BETWEEN YEARS AND PARK TYPES TO CREATE DATA FRAMES ###########
###############################################################################
# AS THE PREVIOUS LOOP, THIS LOOP DO THE SAME BUT FOR LOGITUDINAL DATA TO RUN #
# PANEL MODELS FOR EACH TYPE OF PROTECTED AREA                                #
###############################################################################


for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  for(i in 1:2){
    
    print(paste0("territorio ",i))
    dist_panel <- list()
    for(y in 2001:2016) {
      print(paste0("year ",y))
      dist_temp <- readRDS(paste0("Dataframes/","Estrategia ",d,"/dist_terr",i,"_",y,".rds"))
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
    dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2016 
    dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))
    
    desig_2016 <- dist_panel[dist_panel$year==2016,c("ID","buffer_id","dist")]
    names(desig_2016) <- c("ID","buffer_id_2016","dist_2016")
    dist_panel <- merge(dist_panel, desig_2016, all.x=TRUE, all.y=TRUE, by="ID")
    print(dim(dist_panel))
    saveRDS(dist_panel, file =  paste0("Dataframes/","Estrategia ",d,"/dist_panel_terr",i,".rds"))

    }
}

