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



# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/func_tables.R")
source("modeling/merge_datasets.R")

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))


##############################################################################################
###################################### 1. DEFORESTATION ######################################
##############################################################################################

##############################################################################################
#################################### OPTIMAL ROBUST BANDWITHS ################################
######################################### (NO CONTROLS ) #####################################
##############################################################################################

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


saveRDS(rd_robust_parks_2, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_parks_2"))
saveRDS(rd_robust_terr_2, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_terr_2.rds"))


##############################################################################################
#################################### OPTIMAL ROBUST BANDWITHS ################################
########################################### (CONTROLS) #######################################
##############################################################################################

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


saveRDS(rd_robust_parks_2_ctrl, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_parks_2_ctrl.rds"))
saveRDS(rd_robust_terr_2_ctrl, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_terr_2_ctrl.rds"))


##############################################################################################
################################### 2. SIMCI DATA: COCA CROPS ################################
##############################################################################################

##############################################################################################
#################################### OPTIMAL ROBUST BANDWITHS ################################
########################################### (CONTROLS) #######################################
##############################################################################################

rd_robust_parks_2_coca <- lapply(defo_dist[2:3], function(park){
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


saveRDS(rd_robust_parks_2_coca, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_parks_2_coca.rds"))
saveRDS(rd_robust_terr_2_coca, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_terr_2_coca.rds"))

##############################################################################################
############################### 2. SIMCI DATA: ILLEGAL MINING ################################
##############################################################################################

##############################################################################################
#################################### OPTIMAL ROBUST BANDWITHS ################################
########################################### (CONTROLS) #######################################
##############################################################################################



rd_robust_parks_2_mining <- lapply(defo_dist[2:3], function(park){
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

saveRDS(rd_robust_parks_2_mining, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_parks_2_mining.rds"))
saveRDS(rd_robust_terr_2_mining, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_terr_2_mining.rds"))


