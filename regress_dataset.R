
# Create dataframes for regressions

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
dist_b$DESIG2 <- mapvalues(dist_b$DESIG, levels(dist_b$DESIG), c(1:16))
dim(dist_b)

# pixel by year (stock) and type of park (and also for groups of park types)

national <- c(2,5,8,11,12,13,14,15,16)
regional <- c(1,3,4,6,7,9,10)

for(y in 2000:2012) {

  print(paste0("year ",y))
#  dist_y <- dist_b[dist_b$STATUS_YR < y,]
  eval(parse(text=paste("dist_",y," <- list()", sep="")))
  eval(parse(text=paste("dist_y <- dist_b[dist_b$STATUS_YR < ",y,",]", sep="")))
  print(dim(dist_y))
  
  # For each type 
  for(i in levels(dist_b$DESIG2)){
    print(i)
    dist_temp <- dist_y[dist_y$DESIG2==i,]
    setorder(dist_temp, ID,-treatment,dist)
    dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
    eval(parse(text=paste("dist_",y,"[[i]] <- dist_temp", sep="")))
    }
  eval(parse(text=paste("saveRDS(dist_",y,", file =  paste0(data, \"dist_",y,".rds\"))", sep="")))
  
  # for all types
  print("all parks")
  eval(parse(text=paste("dist_temp <- do.call(rbind, dist_",y,")", sep="")))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  eval(parse(text=paste("saveRDS(dist_temp, file =  paste0(data, \"dist_",y,"_all.rds\"))", sep="")))
  
  # for National managed parks 
  print("national")
  eval(parse(text=paste("dist_temp <- do.call(rbind, dist_",y,"[national])", sep="")))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  eval(parse(text=paste("saveRDS(dist_temp, file =  paste0(data, \"dist_",y,"_national.rds\"))", sep="")))
 
  # for Regional managed parks 
  print("regional")
  eval(parse(text=paste("dist_temp <- do.call(rbind, dist_",y,"[regional])", sep="")))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  eval(parse(text=paste("saveRDS(dist_temp, file =  paste0(data, \"dist_",y,"_regional.rds\"))", sep="")))

  }

########################################################

# Panel: 2001-2012

########################################################


# for all, national, regional


areas <- c("all","national","regional")

for(a in areas) {
  print(paste0("area ",a))
  dist_panel <- list()
  for(y in 2001:2012) {
    print(paste0("year ",y))
    eval(parse(text=paste("dist_temp <- readRDS(paste0(data,\"dist_",y,"_",a,".rds\"))", sep="")))
    dist_temp <- dist_temp[dist_temp$dist<=10000,]
    dist_temp$year <- y
    dist_panel[[y-2000]] <- dist_temp
    }

  dist_panel <- do.call(rbind, dist_panel)
  eval(parse(text=paste("saveRDS(dist_panel, file =  paste0(data, \"dist_panel_",a,".rds\"))", sep="")))
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

