
# Clean treatment and control

############################

rm(list=ls())
library(magrittr)
library(dplyr)
library(plyr)
library(stringr)
library(sp)
library(rgdal)
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

colombia_municipios_df <- read.csv("colombia_raster_df")

# All

dist_2012_c = list()

dist_2012_c[[1]] <- dist_2012[[1]][dist_2012[[1]]$treatment == 0 & dist_2012[[1]]$ID %in% colombia_municipios_df[, "ID"], ]

other_treatments <- dist_terr_2012 %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
    }) %>%
  do.call(c, .)

dist_2012_c[[1]]$valid_control <- ifelse(dist_2012_c[[1]]$ID %in% other_treatments , 0, 1)

# National
dist_2012_c[[2]] <- dist_2012[[2]][dist_2012[[2]]$treatment == 0 & dist_2012[[2]]$ID %in% colombia_municipios_df[, "ID"], ]

other_treatments <- list(dist_2012[[3]], dist_terr_2012[[1]], dist_terr_2012[[2]]) %>%
  lapply(., function(x){
    filter(x, treatment == 1) %>%
      select(., ID) %>%
      .[, "ID"]
  }) %>%
  do.call(c, .)

dist_2012_c[[2]]$valid_control <- ifelse(dist_2012_c[[2]]$ID %in% other_treatments , 0, 1)

# Regional
dist_2012_c[[3]] <- dist_2012[[3]][dist_2012[[3]]$treatment == 0 & dist_2012[[3]]$ID %in% colombia_municipios_df[, "ID"]  , ]

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

dist_terr_2012_c[[1]] <- dist_terr_2012[[1]][dist_terr_2012[[1]]$treatment == 0 & dist_terr_2012[[1]]$ID %in% colombia_municipios_df[, "ID"]  , ]
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
dist_terr_2012_c[[2]] <- dist_terr_2012[[2]][dist_terr_2012[[2]]$treatment == 0 & dist_terr_2012[[2]]$ID %in% colombia_municipios_df[, "ID"] , ]
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

#Function to create distances and treatments to valid controls

distances_to_valid_controls <- function(x, y){
  a <- filter(x, treatment == 1)
  b <- filter(y, valid_control == 1)
  
  dist <- list(dplyr::select(b, c(x, y)), dplyr::select(a, c(x, y))) %>%
    lapply(., function(xx){
      coordinates(xx) <- c("x", "y")
      proj4string(xx) <- CRS("+init=epsg:4326")
      return(spTransform(xx, CRS=CRS("+init=epsg:3857")) %>%
               as.data.frame(.@coordinates))
    })
  
  fit1 <- get.knnx(dist[[1]], dist[[2]], k = 1)
  dist2 <- b[fit1$nn.index, ][, c("ID", "buffer_id")] %>%
    setNames(., nm = c("ID_f", "buffer_id_f")) %>%
    mutate(.,dist_f = fit1$nn.dist[, 1] - 464)
  
  bind <- cbind(a, dist2) %>% mutate(., valid_treatment = ifelse(dist_f <= 50000, 1, 0)) %>%
    as.data.frame()
  
}

# Create lists of data.frames with valid treatments

dist_2012_t <- mapply(distances_to_valid_controls, x = dist_2012 , y = dist_2012_c, SIMPLIFY = FALSE)
dist_terr_2012_t <- mapply(distances_to_valid_controls, x = dist_terr_2012, y = dist_terr_2012_c, SIMPLIFY = FALSE)


setwd("/Users/Ivan/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
mapply(function(x, y){
  write.csv(x, str_c("dist_terr_2012", y, "t",sep = "_"))
} x = dist_terr_2012_t, y = c("indigenous", "communities"))


saveRDS(dist_2012_t, "dist_2012_t.rds")
saveRDS(dist_terr_2012_t, "dist_terr_2012_t.rds")

