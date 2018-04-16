###############################################################################
#           Regression discontinuity tables from RDS objects
# This code will take the rdrobust functions output and convert if to a LaTeX
# code. The process will use the functions in ./R/rd_to_tables.R and will use
# stargazer package to convert summary talbes into LaTeX code
#
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


# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/rd_to_tables.R")

# Set directories
setwd(Sys.getenv("DATA_FOLDER"))


list_df <- c(defo_dist[2:3], defo_dist_terr)
test <- c(rd_robust_parks_2[2:3], rd_robust_terr_2)
df_test <- rd_to_df_2(test, 
           control_df = list_df, 
           names = c("National", "Regional", "Black", "Ingigenous"),
           digits = 4)





mutate_nse <- function(x){
  df_test %>%
    mutate( new_var = UQE(x) * 10)
}

