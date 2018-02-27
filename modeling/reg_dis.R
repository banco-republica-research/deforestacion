##############################################################################################
##############################################################################################
###                          RUN REGRESSION DISCONTINUITY MODELS                           ###
###     THIS CODE WILL READ THE DISTANCE AND DEFORESTATION DATA TO RUN RD MODELS           ###
###     HERE WE WILL RUN THE MODELS FOR BOTH STRATEGIES (1: REMOVE CONTINENTAL BORDERS AND ###
###             2. REMOVE ALL COTREATMENT BORDERS AND USE EFFECTIVE BORDERS).              ###
##############################################################################################
##############################################################################################

rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(stringr)
library(stargazer)
library(foreign)
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)


# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

########################################## STRATEGY 2: EFFECTIVE BORDERS ###############################################

#Import datasets (covariates)
defo <- read.csv(paste0("Dataframes", "/","dataframe_deforestacion.csv")) %>% dplyr::select(-X)
cov <- read.csv(paste0("Dataframes", "/","geographic_covariates.csv"))
treecover <- read.csv(paste0("Dataframes", "/", "treecover_2000.csv")) %>% dplyr::select(ID, treecover_agg)
simci_coca <- read.csv(paste0( "Dataframes", "/", "coca_simci_extract.csv")) %>% dplyr::select(contains("coca"), ID)
simci_mining <- read.csv(paste0("Dataframes", "/", "illegal_mining_simci_extract.csv")) %>% dplyr::select(contains("EVOA"), ID)


list_files <- list.files(paste0("Dataframes", "/", "Estrategia 2"), full.names = TRUE)
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

territories_2000 <- list_files[str_detect(list_files, "_2000")] %>%
  .[str_detect(., "terr")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

#Aggregate deforestation (2001 - 2012) and Coca crops (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(5:length(names(defo)) - 1 )])
loss_sum <- dplyr::select(defo, c(ID, loss_sum)) %>% mutate(loss_sum = loss_sum / 16)
simci_coca$coca_agg <- rowMeans(simci_coca[, c(1:16)], na.rm = T)


#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    # mutate(., buffer_id = as.character(buffer_id)) %>% mutate(., buffer_id = as.factor(buffer_id))
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    merge(., simci_coca, by = "ID", all.x = T) %>%
    merge(., simci_mining, by = "ID", all.x = T)
})

defo_dist_terr <- lapply(territories_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    # mutate(., buffer_id = as.character(buffer_id)) %>% mutate(., buffer_id = as.factor(buffer_id))
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID") %>%
    merge(., simci_coca, by = "ID", all.x = T) %>%
    merge(., simci_mining, by = "ID", all.x = T)
})

#Regression discontinuity for fixed bandwidths (5 and 10 km)
list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_2 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    #covs = cbind(as.factor(as.character(x$buffer_id))),
    vce = "nn",
    h = 5,
    nnmatch = 8,
    all = T
  )
})

rd_robust_fixed_ten_2 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(as.factor(as.character(x$buffer_id))),
    vce = "nn",
    nnmatch = 8,
    h = 10,
    all = T
  )
})


#Regression discontinuity (optimal bandwidth)

rd_robust_parks_2 <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 8,
    all = T
  )
})


rd_robust_terr_2 <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = terr$loss_sum,
    x = terr$dist_disc,
    covs = cbind(as.factor(as.character(terr$buffer_id))),
    vce = "nn",
    nnmatch = 8,
    all = T
  )
})

setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_parks_2, "rd_robust_parks_2.rds")
saveRDS(rd_robust_terr_2, "rd_robust_terr_2.rds")


#RD with controls for fixed bandwiths (5km and 10km)

list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_ctrl_2 <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten_ctrl_2 <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})


#Regression discontinuity with controls (optimal bandwidth)

rd_robust_parks_2_ctrl <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


rd_robust_terr_2_ctrl <- lapply(defo_dist_terr, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T
  )
})

setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_parks_2_ctrl, "rd_robust_parks_2_ctrl.rds")
saveRDS(rd_robust_terr_2_ctrl, "rd_robust_terr_2_ctrl.rds")

rd_robust_parks_2_ctrl <- readRDS("rd_robust_parks_2_ctrl.rds")
rd_robust_terr_2_ctrl <- readRDS("rd_robust_terr_2_ctrl.rds")


############################# SIMCI DATA ##################################

#Regression discontinuity with controls (optimal bandwidth)

rd_robust_parks_2_coca <- lapply(defo_dist[1:2], function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})

rd_robust_terr_2_coca <- lapply(defo_dist_terr, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



rd_robust_parks_2_mining <- lapply(defo_dist[1:2], function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T
  )
})

rd_robust_terr_2_mining <- lapply(defo_dist_terr, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$clumps_1, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T
  )
})




saveRDS(rd_robust_terr_2_coca, paste0("Backup Data", "/", "rd_robust_terr_2_coca.rds"))
saveRDS(rd_robust_terr_2_mining, paste0("Backup Data", "/", "rd_robust_terr_2_mining.rds"))

saveRDS(rd_robust_parks_2_coca, paste0("Backup Data", "/", "rd_robust_parks_2_coca.rds"))
saveRDS(rd_robust_parks_2_mining, paste0("Backup Data", "/", "rd_robust_parks_2_mining.rds"))



############################################# STRATEGY 1: ALL BORDERS ###################################################
#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv") %>% dplyr::select(-X)
cov <- read.csv("geographic_covariates.csv") %>% dplyr::select(-X)
clump <- read.csv("clump_id_dataframe_2000.csv") %>% dplyr::select(ID, clumps)


setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 1")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

territories_2000 <- list_files[str_detect(list_files, "_2000")] %>%
  .[str_detect(., "terr")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)) - 1 )])
loss_sum <- dplyr::select(defo, c(ID, loss_sum)) %>% mutate(loss_sum = loss_sum / 12)

#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID")
})

defo_dist_terr <- lapply(territories_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., treecover, by = "ID")
})

#Regression discontinuity for fixed bandwidths (5 and 10 km)
list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_1 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    vce = "nn",
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten_1 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    vce = "nn",
    all = T,
    h = 10
  )
})


#Regression discontinuity (optimal bandwidth)

rd_robust_parks_1 <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


rd_robust_terr_1 <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = terr$loss_sum,
    x = terr$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})

setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_parks_1, "rd_robust_parks_1.rds")
saveRDS(rd_robust_terr_1, "rd_robust_terr_1.rds")

#RD with controls for fixed bandwiths (5km and 10km)

list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_ctrl_1 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$prec, 
                 x$sq_1km.1, x$treecover_agg, x$clumps_1),
    vce = "nn",
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten_ctrl_1 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$prec, 
                 x$sq_1km.1, x$treecover_agg, x$clumps_1),
    vce = "nn",
    all = T,
    h = 10
  )
})



#Regression discontinuity (optimal bandwidth) with controls

rd_robust_parks_1_ctrl <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, park$clumps_1),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


rd_robust_terr_1_ctrl <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = terr$loss_sum,
    x = terr$dist_disc,
    covs = cbind(terr$altura_tile_30arc, terr$slope, terr$roughness, terr$prec, 
                 terr$sq_1km.1, terr$treecover_agg, terr$clumps_1),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})

setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_parks_1_ctrl, "rd_robust_parks_1_ctrl.rds")
saveRDS(rd_robust_terr_1_ctrl, "rd_robust_terr_1_ctrl.rds")


#################################################### LATEX TABLES ######################################################
############################################## RD OBJECT TO DATAFRAME FUNCTION ########################################
rd_to_df <- function(list, dataframe){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_h_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_h_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1])))
  
  defo_mean <- mapply(function(x, y){
    y %>%
      filter(abs(dist_disc) <= x$bws[1, 1] & treatment == 0) %>% 
      summarize(mean = mean(loss_sum))
  }, x = list , y = dataframe, SIMPLIFY = F) %>% unlist()
  
  df <- rd %>% cbind(., defo_mean) %>% t() %>%
     as.data.frame() %>% 
     dplyr::rename(Nacionales = V1,
                                       Regionales = V2, Resguardos = V3,
                                       Comunidades = V4) %>%
   mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric))
   row.names(df) <- c("Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "Media control")
  return(df)
}
######################################################################################################################

#Strategy 2 - Fixed bw's

df_five <- rd_to_df(rd_robust_fixed_five_2, list_df)
df_ten <- rd_to_df(rd_robust_fixed_ten_2, list_df)
df_five_ctrl <- rd_to_df(rd_robust_fixed_five_ctrl_2, list_df)
df_ten_ctrl <- rd_to_df(rd_robust_fixed_ten_ctrl_2, list_df)

df_five_final <- cbind(df_five, df_five_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
df_ten_final <- cbind(df_ten, df_ten_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_five_final, df_ten_final, summary = F, decimal.mark = ",", digits = 3, digit.separator = ".")

#Strategy 2 - Optimal bw's

rd_optimal <- c(rd_robust_parks_2[2:3], rd_robust_terr_2)
df_optimal <- rd_to_df(rd_optimal, list_df)

rd_optimal_ctrl <- c(rd_robust_parks_2_ctrl[2:3], rd_robust_terr_2_ctrl)
df_optimal_ctrl <- rd_to_df(rd_optimal_ctrl, list_df) 
df_optimal_final <- cbind(df_optimal, df_optimal_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_optimal_final, summary = F, decimal.mark = ",", digits = 3, digit.separator = ".")

#Strategy 1 - Fixed bw's

df_five <- rd_to_df(rd_robust_fixed_five_1, list_df)
df_ten <- rd_to_df(rd_robust_fixed_ten_1, list_df)
df_five_ctrl <- rd_to_df(rd_robust_fixed_five_ctrl_1, list_df)
df_ten_ctrl <- rd_to_df(rd_robust_fixed_ten_ctrl_1, list_df)

df_five_final <- cbind(df_five, df_five_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
df_ten_final <- cbind(df_ten, df_ten_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_five_final, df_ten_final, summary = F, decimal.mark = ",", digits = 3, digit.separator = ".")


#Strategy 1 - Optimal bw's

rd_optimal <- c(rd_robust_parks_1[2:3], rd_robust_terr_1)
df_optimal <- rd_to_df(rd_optimal, list_df)

rd_optimal_ctrl <- c(rd_robust_parks_1_ctrl[2:3], rd_robust_terr_1_ctrl)
df_optimal_ctrl <- rd_to_df(rd_optimal_ctrl, list_df) 
df_optimal_final <- cbind(df_optimal, df_optimal_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_optimal_final, summary = F, decimal.mark = ",", digits = 3, digit.separator = ".")


################################################# HETEROGENEUS EFFECTS #################################################
###################################################### STRATEGY 2 ######################################################

#Heterogeneus effects by clump and fixed bw's (5 km)

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 1))
rd_robust_fixed_five_clump1_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})

list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(function(x) filter(x, roads == 0))
rd_robust_fixed_five_clump0_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})


#Heterogeneus effects by clump and fixed bw's (10 km)

list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 1))
rd_robust_fixed_ten_clump1_2 <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})

list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(function(x) filter(x, roads == 0))
rd_robust_fixed_ten_clump0_2 <-  lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})


#Heterogeneus effects by clump and optimal bw's

list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 1))
rd_robust_clump1_2 <- lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 0))
rd_robust_clump0_2 <- lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_clump1_2, "rd_robust_roads1_2.rds")
saveRDS(rd_robust_clump0_2, "rd_robust_roads0_2.rds")


################################################# HETEROGENEUS EFFECTS #################################################
###################################################### STRATEGY 2 ######################################################
#################################################### - SIMCI DATA - ####################################################


#Heterogeneus effects by clump and fixed bw's for coca crops (5 km)

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 1))
rd_robust_fixed_five_clump1_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(function(x) filter(x, clumps_1 == 0))
rd_robust_fixed_five_clump0_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})


saveRDS(rd_robust_fixed_five_clump1_2_coca, 
        paste0("Deforestacion", "/", "Results", "/", "RD", "/", "Models", "/" ,"rd_robust_fixed_five_clump1_2_coca"))
saveRDS(rd_robust_fixed_five_clump0_2_coca, 
        paste0("Deforestacion", "/", "Results", "/", "RD", "/","Models", "/" ,"rd_robust_fixed_five_clump0_2_coca"))

#Heterogeneus effects by clump and fixed bw's for illegal mining (5 km)

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 1))
rd_robust_fixed_five_clump1_2_mining <-  lapply(list_df, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(function(x) filter(x, clumps_1 == 0))
rd_robust_fixed_five_clump0_2_mining <-  lapply(list_df, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 5
  )
})


saveRDS(rd_robust_fixed_five_clump1_2_mining, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_five_clump1_2_mining"))
saveRDS(rd_robust_fixed_five_clump0_2_mining, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_five_clump0_2_mining"))

#Heterogeneus effects by clump and fixed bw's for coca crops (10 km)

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 1))
rd_robust_fixed_ten_clump1_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(function(x) filter(x, clumps_1 == 0))
rd_robust_fixed_ten_clump0_2_coca <-  lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})


saveRDS(rd_robust_fixed_ten_clump1_2_coca, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_ten_clump1_2_coca"))
saveRDS(rd_robust_fixed_ten_clump0_2_coca, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_ten_clump0_2_coca"))


#Heterogeneus effects by clump and fixed bw's for illegal mining (10 km)

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 1))
rd_robust_fixed_ten_clump1_2_mining <-  lapply(list_df, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(function(x) filter(x, roads == 0))
rd_robust_fixed_five_clump0_2_mining <-  lapply(list_df, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    c = 0,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    all = T,
    h = 10
  )
})

saveRDS(rd_robust_fixed_ten_clump1_2_mining, 
        paste0("Deforestacion","/",  "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_ten_clump1_2_mining"))
saveRDS(rd_robust_fixed_ten_clump0_2_mining, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_fixed_ten_clump0_2_mining"))


#Heterogeneus effects by clump and optimal bw's for Coca Crops

list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 1))
rd_robust_clump1_2_coca <- lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 8,
    all = T
  )
})



list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 0))
rd_robust_clump0_2_coca <- lapply(list_df, function(park){
  rdrobust(
    y = park$coca_agg,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


saveRDS(rd_robust_clump1_2_coca , 
        paste0("Deforestacion","/",  "Results", "/","RD", "/", "Models", "/" ,"rd_robust_clump1_2_coca"))
saveRDS(rd_robust_clump0_2_coca , 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_clump0_2_coca"))


#Heterogeneus effects by clump and optimal bw's for Illegal mining
counter <- 0
list_df <- c(defo_dist[2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 1))
rd_robust_clump1_2_mining <- lapply(list_df, function(park){
  counter <<- counter + 1
  print(counter)
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



list_df <- c(defo_dist[1:2], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, clumps_1 == 0))
rd_robust_clump0_2_mining <- lapply(list_df, function(park){
  rdrobust(
    y = park$illegal_mining_EVOA_2014,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


saveRDS(rd_robust_clump1_2_mining, 
        paste0("Deforestacion","/",  "Results", "/","RD", "/", "Models", "/" ,"rd_robust_clump1_2_mining"))
saveRDS(rd_robust_clump0_2_mining, 
        paste0("Deforestacion", "/", "Results", "/","RD", "/", "Models", "/" ,"rd_robust_clump0_2_mining"))



#Heterogeneus effects by clump and optimal bw's

list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 1))
rd_robust_clump1_2 <- lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



list_df <- c(defo_dist[2:3], defo_dist_terr) %>%
  lapply(., function(x) base::subset(x, roads == 0))
rd_robust_clump0_2 <- lapply(list_df, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$prec, 
                 park$sq_1km.1, park$treecover_agg, as.factor(as.character(park$buffer_id))),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})



setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_clump1_2, "rd_robust_roads1_2.rds")
saveRDS(rd_robust_clump0_2, "rd_robust_roads0_2.rds")



df_optimal_het1 <- rd_to_df(rd_robust_clump1_2, list_df)[c("Tratamiento", "StdErr", "p", "N", "bw", "Media control"), ]
df_optimal_het0 <- rd_to_df(rd_robust_clump0_2, list_df)[c("Tratamiento", "StdErr", "p",  "N", "bw", "Media control"), ]

df_optimal_final <- rbind(df_optimal_het0, df_optimal_het1)
stargazer(df_optimal_final, summary = F, decimal.mark = ",", digits = 3, digit.separator = ".")

