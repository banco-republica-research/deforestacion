##############################################################################################
##############################################################################################
###                          RUN REGRESSION DISCONTINUITY MODELS                           ###
###     THIS CODE WILL READ THE DISTANCE AND DEFORESTATION DATA TO RUN RD MODELS           ###
###     HERE WE WILL RUN THE MODELS FOR BOTH STRATEGIES (1: REMOVE CONTINENTAL BORDERS AND ###
###             2. REMOVE ALL COTREATMENT BORDERS AND USE EFFECTIVE BORDERS).              ###
##############################################################################################
##############################################################################################

##############################################################################################
############################## FIXED BANDWIDTHS: 5 KM AND 10 KM ##############################
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
############################## FIXED BANDWIDTHS: 5 KM AND 10 KM ##############################
######################################### (NO CONTROLS ) #####################################
##############################################################################################

list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_2 <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
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
    vce = "nn",
    nnmatch = 8,
    h = 10,
    all = T
  )
})


##############################################################################################
############################## FIXED BANDWIDTHS: 5 KM AND 10 KM ##############################
########################################### (CONTROLS) #######################################
##############################################################################################

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

saveRDS(rd_robust_fixed_five_ctrl_2, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_fixed_five_ctrl_2"))
saveRDS(rd_robust_fixed_ten_ctrl_2, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/rd_robust_fixed_ten_ctrl_2.rds"))


