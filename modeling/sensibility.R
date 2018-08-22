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
library(tidyr)


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

# List of dataframes with data per each area: parks and territories
list_df <- c(defo_dist[2:3], defo_dist_terr)
data_tibble <- tibble::tibble(data = list_df) %>%
  mutate(dfs = c('National', 'Regional', 'Black', 'Indigenous'))

# Extract optimal bw's for all areas and selected models

selected_models <- list(df_robusts_controls,
                        df_robust_coca_control,
                        df_robust_mining)

lists_bws_optimal <- lapply(selected_models, function(x){
  x %>%
    t() %>%
    as.data.frame() %>%
    select(bws) %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric) %>%
    t() %>%
    set_colnames(c('National', 'Regional', 'Indigenous', 'Black'))
}) %>% ldply() %>%
  mutate(estimated_var = c('loss_sum', 'coca_agg', 'mining')) %>%
  gather(dfs, optimal_bw, National:Black)

# Dataframe with iteration paramters 
iter_df  <- cross3(c('National', 'Regional', 'Black', 'Indigenous'),
                              c('loss_sum', 'coca_agg', 'mining'),
                              c('loss_sum', 'coca_agg', 'mining')) %>%
  map(setNames, c('dfs', 'bws', 'vars')) %>%
  bind_rows() %>%
  filter(bws==vars) %>%
  arrange(dfs) %>%
  left_join(.,
            lists_bws_optimal,
            by=c('dfs'='dfs', 'bws'='estimated_var'))

iter_df_data <- iter_df %>%
  inner_join(., data_tibble, by = c('dfs'))

###############################################################################
###################### SENSIBILITY TESTS TO ALL AREAS  ########################
###############################################################################


if('sensibility_all.rds' %in% list.files()){
  print(paste0('Sensibility tests already calculated and stored in', getwd()))
  sensibility_all <- readRDS('sensibility_all.rds')
} else {
  sensibility_all <- cross3(list_df, lists_bws_optimal, vars) %>% 
    invoke_map("rd_sensibility", ., start = 1,  end = 15, step = 0.5)
  
  saveRDS(sensibility_all, 'sensibility_all.rds')
}
 
###############################################################################
############################ SENSIBILITY TESTS TO DF  ########################
###############################################################################

# Get list with bw's



sensibility_all_df <- sensibility_all %>%
  unlist(recursive=F) %>% 
  unlist(recursive = F) %>%
  lapply(extract_values) %>%
  

test <- cbind(df_names, sensibility_all_df)

