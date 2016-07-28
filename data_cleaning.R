
# Clean treatment and control

############################

rm(list=ls())
library(magrittr)
library(plyr)
library(stringr)
library(dplyr)
library(FNN)

# Leonardo
# setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/Datos/Dataframes/")
# Ivan 
setwd("/Users/Ivan/Dropbox/BANREP/Deforestacion/Datos/Dataframes")

########################################################

# Valid controls  

########################################################

list_files <- list.files()
dist_2012 <- list_files[str_detect(list_files, "dist_2012")] %>%
  .[str_detect(., "al")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

dist_terr_2012 <- list_files[str_detect(list_files, "_2012")] %>%
  .[str_detect(., "terr")] %>%
  .[c(2,3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame) 

# All

dist_2012_c = list()

dist_2012_c[[1]] <- dist_2012[[1]][dist_2012[[1]]$treatment == 0, ]

other_treatments <- dist_terr_2012 %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
    }) %>%
  do.call(c, .)

dist_2012_c[[1]]$valid_control <- ifelse(dist_2012_c[[1]]$ID %in% other_treatments , 0, 1)

# National
dist_2012_c[[2]] <- dist_2012[[2]][dist_2012[[2]]$treatment == 0, ]

other_treatments <- list(dist_2012[[3]], dist_terr_2012[[1]], dist_terr_2012[[2]]) %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
  }) %>%
  do.call(c, .)

dist_2012_c[[2]]$valid_control <- ifelse(dist_2012_c[[2]]$ID %in% other_treatments , 0, 1)

# Regional
dist_2012_c[[3]] <- dist_2012[[3]][dist_2012[[3]]$treatment == 0, ]

other_treatments <- list(dist_2012[[2]], dist_terr_2012[[1]], dist_terr_2012[[2]]) %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
  }) %>%
  do.call(c, .)

dist_2012_c[[3]]$valid_control <- ifelse(dist_2012_c[[3]]$ID %in% other_treatments , 0, 1)

# Indigenous 
dist_terr_2012_c = list()

dist_terr_2012_c[[1]] <- dist_terr_2012[[1]][dist_terr_2012[[1]]$treatment == 0, ]
other_treatments <- list(dist_2012[[1]], dist_terr_2012[[2]]) %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%  
  filter(., treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
}) %>%
  do.call(c, .)

dist_terr_2012_c[[1]]$valid_control <- ifelse(dist_terr_2012_c[[1]]$ID %in% other_treatments , 0, 1)

# Black
dist_terr_2012_c[[2]] <- dist_terr_2012[[2]][dist_terr_2012[[2]]$treatment == 0, ]
other_treatments <- list(dist_2012[[1]], dist_terr_2012[[1]]) %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%  
      filter(., treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
  }) %>%
  do.call(c, .)

dist_terr_2012_c[[2]]$valid_control <- ifelse(dist_terr_2012_c[[2]]$ID %in% other_treatments , 0, 1)

saveRDS(dist_2012_c, "dist_2012_c.rds")
saveRDS(dist_terr_2012_c, "dist_terr_2012_c.rds")


########################################################

# Treatment near valid controls  

########################################################


# All
dist_2012_t = list()
dist_2012_t[[1]] <- dist_2012[[1]][dist_2012[[1]]$treatment == 1, ]
frontier <- dist_2012_c[[1]][dist_2012_c[[1]]$dist < 100,]

lmat0 <- frontier[c("x","y")]
lmat1 <- dist_2012_t[[1]][c("x","y")]

### Calculating distance to the other
fit2 <- get.knnx(lmat0,lmat1,k=1) #for treat==1, get those treat==0 that are K=1 nearest neighbours 
lmat2 <- lmat0[fit2$nn.index,] # keep only those treat==0 with treat==1 k=1 nearest neighbours
long1 <- 2*pi*lmat1$x/360
lat1 <- 2*pi*lmat1$y/360
long2 <- 2*pi*lmat2$x/360
lat2 <- 2*pi*lmat2$y/360
dist <- acos(pmin(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(long2-long1),  1))*3958

dist_2012_t[[1]]$frontier_ID <- lmat2
dist_2012_t[[1]]$dist_frontier <- dist
summary(dist_2012_t[[1]])

cor(dist_2012_t[[1]]$dist, dist_2012_t[[1]]$dist_frontier)


