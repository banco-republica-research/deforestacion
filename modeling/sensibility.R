###############################################################################
##################### SENSIBILITY TESTS FOR MAIN RESULTS ######################
###############################################################################

rm(list=ls())
library(plyr)
library(dplyr)
library(rdrobust)
library(stringr)
library(stargazer)
library(ggplot2)
library(magrittr)
library(foreign)
library(purrr)
library(rlang)


# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/rd_functions.R")
source("R/func_tables.R")
source("R/aesthetics.R")

# Load optimal bw's
source("modeling/rd_to_tables.R")

# Set directories
setwd(Sys.getenv("OUTPUT_FOLDER"))

###############################################################################
##################### SENSIBILITY TESTS FOR MAIN RESULTS ######################
######################### ALL AREAS - SELECTED MODELS  ########################
## BE AWARE THAT THIS CODE WILL ONLY RUN SENSIBILIY CHECKS TO THE MODELS IN  ##
## THE LIST. THIS MODELS CAN BE SELECTED USING THE NAMES OF THE RESULTS DATA ##
## FRAME IN THE FILE modeling/rd_to_tables.R                                 ##
###############################################################################

# Extract optimal bw's for all areas and selected models

selected_models <- list(df_robusts_controls,
                        df_robust_coca_control,
                        df_robust_mining)

lists_bws_optimal <- sapply(selected_models, function(x){
  x %>%
    t() %>%
    as.data.frame() %>%
    select(bws) %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric)
})

# Define vars to run sensibility tests
list_df <- c(defo_dist[2:3], defo_dist_terr)
vars <- c('loss_sum', 'coca_agg', 'illegal_mining_EVOA_2014')

# Run sensibility tests

sensibility_all <- cross3(1:4, 1:3, 1:3) %>% 
  invoke_map("paste", .)


sensibility_all <- cross3(list_df, lists_bws_optimal , vars) %>% 
  invoke_map("rd_sensibility", ., start = 1,  end = 15, step = 0.5)


saveRDS(sensibility_all, 'sensibility_all.rds')

