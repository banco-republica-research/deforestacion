
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

  
# pixel by year (stock) and type of park (and also for groups of park types)
all <- c(1:15)
national <- c(2,5,7,8,11,12,13,14,15)
regional <- c(1,3,4,6,10)
private <- 9
areas <- c("all","national","regional")

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

  for(y in 2000:2012) {
    
    print(paste0("year ",y))
    dist_yl <- list()
    eval(parse(text=paste("dist_y <- dist_b[dist_b$year < ",y,",]", sep="")))
    print(dim(dist_y))
    
    # For each park
    for(i in levels(dist_b$DESIG2)){
      print(i)
      dist_temp <- dist_y[dist_y$DESIG2==i,]
      #    print(dim(dist_temp))
      setorder(dist_temp, ID,-treatment,dist)
      dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
      dist_yl[[i]] <- dist_temp
    }
    saveRDS(dist_yl, file =  paste0(data,"Estrategia ",d,"/dist_",y,".rds"))

    # By type of area 
    for(a in areas) {
      print(paste(a, "Parks"))
      eval(parse(text=paste("dist_temp <- dist_yl[",a,"]", sep="")))
      dist_temp <- do.call(rbind, dist_temp)
      setorder(dist_temp, ID,-treatment,dist)
      dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
      saveRDS(dist_temp, file =  paste0(data, "Estrategia ",d,"/dist_",y,"_",a,".rds"))
    }
  }
}


########################################################

# Panel: 2001-2012

########################################################

# for all, national, regional

areas <- c("all","national","regional")

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  for(a in areas) {
  print(paste0("area ",a))
  dist_panel <- list()
  for(y in 2001:2012) {
    print(paste0("year ",y))
    dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_",y,"_",a,".rds"))
    dist_temp <- dist_temp[dist_temp$dist<=20000,]
    dist_temp$year <- y
    dist_panel[[y-2000]] <- dist_temp
    }
  dist_panel <- do.call(rbind, dist_panel)

  # Balanced panel: 
  # Borrar ID = 0 
  # treatment = 0 if park did not exit in year 
  # desig_2012 = Time-invariant type of park (Use the 2012 park)
  # Desig_first = first year of desig if multiple 
  
  dist_panel <- dist_panel[dist_panel$ID!=0, ]
  iddat <- expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
  dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
  dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 

  dist_panel$desig_first <- dist_panel$STATUS_YR
  dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2012 
  dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))

  desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","buffer_id","dist","DESIG2")]
  names(desig_2012) <- c("ID","buffer_id_2012","dist_2012","DESIG2_2012")
  dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")

  print(dim(dist_panel))
  saveRDS(dist_panel, file =  paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
  }
}


# for each area (1-15)

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  for(a in 1:15) {
    print(paste0("area ",a))
    dist_panel <- list()
    for(y in 2001:2012) {
      print(paste0("year ",y))
      dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_",y,".rds"))[[a]]
      if ((dim(dist_temp)[1]) > 0){
        dist_temp <- dist_temp[dist_temp$dist<=20000,]
        dist_temp$year <- y
        dist_panel[[y-2000]] <- dist_temp
      }
    }
    
    if (length(dist_panel)>1) {
      dist_panel <- do.call(rbind, dist_panel)
      
      # Balanced panel: treatment = 0 if park did not exit in year y, and Time-invariant type of park and distance (Use the 2012 park)
      iddat <- expand.grid(ID = unique(dist_panel$ID), year = c(2001:2012))
      dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
      dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 

      dist_panel$desig_first <- dist_panel$STATUS_YR
      dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2012 
      dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))
      
      desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","buffer_id","dist","DESIG2")]
      names(desig_2012) <- c("ID","buffer_id_2012","dist_2012","DESIG2_2012")
      dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")
      print(dim(dist_panel))
      saveRDS(dist_panel, file =  paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
    }
  }
}



########################################################

# Miscellaneous 

########################################################

# pixel as any type of park in 2000

dist_2000_i <- list()

for(i in levels(dist_2000$DESIG2)){
  print(i)
  dist_temp <- dist_2000[dist_2000$DESIG2==i,]
  dist_temp$dup <- duplicated(dist_temp$ID)
  print(table(dist_temp$dup))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  dist_2000_i[[i]] <- dist_temp
}

# pixel by year (stock): all parks 


for(y in 2000:2012) {
  # for all types
  print(paste0("year ",y))
  eval(parse(text=paste("dist_temp <- do.call(rbind, dist_",y,")", sep="")))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  eval(parse(text=paste("saveRDS(dist_temp, file =  paste0(data, \"dist_",y,"_all.rds\"))", sep="")))
} 

# Reopen dist big files
for(y in 2000:2012) {
  print(paste0("year ",y))
  eval(parse(text=paste("dist_",y," <- readRDS(paste0(data,\"dist_",y,".rds\"))", sep="")))
  }  


# Regional parks in all dataset (2000)

regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion",
              "Reserva Forestal Protectora Nacional")
defo_dist$regional <- ifelse(defo_dist$DESIG %in% regional, 1 , 0)

# Panel for all

for(y in 2001:2012) {
  print(paste0("year ",y))
  eval(parse(text=paste("dist_temp <- readRDS(paste0(data,\"dist_",y,"_all.rds\"))", sep="")))
  dist_temp <- dist_temp[dist_temp$dist<=10000,]
  dist_temp$year <- y
  print(dim(dist_temp))
  dist_panel[[y-2000]] <- dist_temp
}

dist_panel <- do.call(rbind, dist_panel)
dim(dist_panel)
saveRDS(dist_panel, file =  paste0(data, "dist_panel_all.rds"))


# Create balanced panel
dist_panel <- readRDS(paste0(data,"dist_panel_all.rds"))
table(dist_panel$year)

iddat = expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
table(dist_panel$treatment, useNA = "always")

dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 
table(dist_panel$treatment, useNA = "always")

desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","DESIG2")]
names(desig_2012) <- c("ID","DESIG2_2012")
dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")



