###############################################################################
###############################################################################
##            REGRESSION DISCONTINUITY TABLES FROM RD_OBJECTS                ##
## This code will take the rdrobust functions output and convert if to a     ##
## LaTeXcode. The process will use the functions in ./R/rd_to_tables.R and   ##
## will use stargazer package to convert summary talbes into LaTeX code      ##
##                                                                           ##
###############################################################################
###############################################################################

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
library(rlang)


# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/func_tables.R")
source("modeling/merge_datasets.R")

# Set directories
setwd(Sys.getenv("OUTPUT_FOLDER"))



###############################################################################
###### RESULTS DATAFRAME PER TABLE: DEFORESTATION FOR EACH PROTECTED AREA #####
################################## (TABLE 3) ##################################
###############################################################################

list_files <- list.files("RD/Models/new_results/", full.names = T)
rd_robust_2_controls <- list_files[str_detect(list_files, '_2_ctrl')] 

list_df <- c(defo_dist, defo_dist_terr)
df_robusts_controls <- rd_to_df_2(rd_robust_2_controls, 
           control_df = list_df, 
           names = c("All", "National", "Regional", "Black", "Ingigenous"),
           digits = 4,
           stargazer = F,
           baseline_variable = "loss_sum", 
           latex = TRUE)


###############################################################################
###### RESULTS DATAFRAME PER TABLE: DEFORESTATION FOR EACH PROTECTED AREA #####
################################# (TABLE A2) ##################################
###############################################################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_2 <- list_files[str_detect(list_files, '_2.rds')] 
list_df <- c(defo_dist, defo_dist_terr)
df_robust <- rd_to_df_2(rd_robust_2, 
                      control_df = list_df, 
                      names = c("All", "National", "Regional", "Black", "Ingigenous"),
                      digits = 4,
                      baseline_variable = "loss_sum",
                      latex = TRUE)


###############################################################################
###### RESULTS DATAFRAME PER TABLE: DEFORESTATION FOR EACH PROTECTED AREA #####
################ HETEROGENEUS EFFECTS FOR LIGHTS AND ROADS ####################
################################## (TABLE 5) ##################################
###############################################################################

################################## LIGHTS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_het_effects_lights <- list_files[str_detect(list_files, regex("robust_clump0\\.|robust_clump1\\."))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[2:3], defo_dist_terr)
df_clumps_list <- lapply(rd_robust_het_effects_lights, function(x){
  rd_to_df_2(x[2:5], 
             control_df = list_df, 
             names = c("National", "Regional", "Black", "Ingigenous"),
             digits = 4,
             baseline_variable = "loss_sum",
             latex = TRUE)
})
                     
################################## ROADS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_het_effects_roads <- list_files[str_detect(list_files, regex("robust_roads0\\.|robust_roads1\\."))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[2:3], defo_dist_terr)
df_roads_list <- lapply(rd_robust_het_effects_roads, function(x){
  rd_to_df_2(x[2:5], 
             control_df = list_df, 
             names = c("National", "Regional", "Black", "Ingigenous"),
             digits = 4,
             baseline_variable = "loss_sum",
             latex = TRUE)
})


###############################################################################
####### RESULTS DATAFRAME PER TABLE: COCA CROPS FOR EACH PROTECTED AREA #######
###############################################################################

list_files <- list.files("RD/Models/new_results/", full.names = T)
rd_robust_2_coca <- list_files[str_detect(list_files, '_2_coca')] %>%
  lapply(., readRDS) %>% unlist(., recursive = F)

list_df <- c(defo_dist[1:2], defo_dist_terr)
df_robust_coca_control <- rd_to_df_2(rd_robust_2_coca, 
                      control_df = list_df, 
                      names = c("All", "National", "Black", "Ingigenous"),
                      digits = 4,
                      baseline_variable = "coca_agg",
                      latex = TRUE)


###############################################################################
###### RESULTS DATAFRAME PER TABLE: MINING 2014 FOR EACH PROTECTED AREA #######
###############################################################################

list_files <- list.files("RD/Models/new_results/", full.names = T)
rd_robust_2_mining <- list_files[str_detect(list_files, '2_mining')] %>%
  lapply(., readRDS) %>% unlist(., recursive = F)

list_df <- c(defo_dist[1:2], defo_dist_terr)
df_robust_mining <- rd_to_df_2(rd_robust_2_mining, 
                                     control_df = list_df, 
                                     names = c("All", "National", "Black", "Ingigenous"),
                                     digits = 4,
                                     baseline_variable = "illegal_mining_EVOA_2014",
                                     latex = TRUE)



###############################################################################
########## RESULTS DATAFRAME PER TABLE: COCA FOR EACH PROTECTED AREA ##########
################ HETEROGENEUS EFFECTS FOR LIGHTS AND ROADS ####################
###############################################################################

################################## LIGHTS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_coca_het_effects_lights <- list_files[str_detect(list_files, regex("robust_clump0_coca|robust_clump1_coca"))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[1:2], defo_dist_terr)
df_clumps_coca_list <- lapply(rd_robust_coca_het_effects_lights, function(x){
  rd_to_df_2(x, 
             control_df = list_df, 
             names = c("All", "National", "Black", "Ingigenous"),
             digits = 4,
             baseline_variable = "coca_agg",
             latex = TRUE)
})

################################## ROADS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_het_effects_roads <- list_files[str_detect(list_files, regex("robust_roads0_coca\\.|robust_roads1_coca\\."))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[2:3], defo_dist_terr)
df_roads_list <- lapply(rd_robust_het_effects_roads, function(x){
  rd_to_df_2(x[2:5], 
             control_df = list_df, 
             names = c("National", "Regional", "Black", "Ingigenous"),
             digits = 4,
             baseline_variable = 'coca_agg',
             latex = TRUE)
})




###############################################################################
######## RESULTS DATAFRAME PER TABLE: MINING FOR EACH PROTECTED AREA ##########
################ HETEROGENEUS EFFECTS FOR LIGHTS AND ROADS ####################
###############################################################################

################################## LIGHTS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_coca_het_effects_lights <- list_files[str_detect(list_files, regex("robust_clump0_mining|robust_clump1_mining"))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[1:2], defo_dist_terr)
df_clumps_coca_list <- lapply(rd_robust_coca_het_effects_lights, function(x){
  rd_to_df_2(x, 
             control_df = list_df, 
             names = c("All", "National", "Black", "Ingigenous"),
             digits = 4,
             latex = TRUE)
})

################################## ROADS ##################################

list_files <- list.files("RD/Models/new_results", full.names = T)
rd_robust_het_effects_roads <- list_files[str_detect(list_files, regex("robust_roads0_coca\\.|robust_roads1_coca\\."))] %>%
  lapply(., readRDS) 

list_df <- c(defo_dist[2:3], defo_dist_terr)
df_roads_list <- lapply(rd_robust_het_effects_roads, function(x){
  rd_to_df_2(x[2:5], 
             control_df = list_df, 
             names = c("National", "Regional", "Black", "Ingigenous"),
             digits = 4,
             latex = TRUE)
})

###############################################################################
##################### GGPLOT FOR ALL VARIABLES AND AREAS ######################
###############################################################################

defo_dist_all <- c(defo_dist[2:3], defo_dist_terr) #Collapse all dataframes into one list and remove "all"

mapply(rd_to_plot, 
       variable = c("loss_sum", "coca_agg", "illegal_mining_EVOA_2014"), 
       variable_name = c("Deforestation (ha/km^2)", "Coca crops (ha/km^2)", "Gold mining (ha/km^2)"))






