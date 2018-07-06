##############################################################################################
##############################################################################################
###                           PERMUTATION TESTS FOR RD MODELS                              ###
### PERMUTATION TEST FOR ALL COVARIATES ACROSS DISCONTINUITIES. AS ALL THE OTHER MODELING  ###
### WE USE TWO LISTS, ONE FOR PARKS (ALL, NATIONAL, AND REGIONAL), AND ONE FOR TERRITORIES ###
### INDIGENOUS AND BLACK. WITH THESE WE CALCULATE IF THERE ARE DISCONTIUTIES ACROSS THE    ###
### BOUNDARIES IN THE COVARIATES. THIS IS AN ACID TEST FOR RD AND MANIPULATION             ###
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
library(rlang)
library(tidyr)
library(RATest)

# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/rd_functions.R")
source("modeling/merge_datasets.R")

# Set directories
setwd(Sys.getenv("OUTPUT_FOLDER"))

# Define function parameters 

list_df <- c(defo_dist[2:3], defo_dist_terr)
areas <- c("National", "Regional", "Black", "Indigenous")
vars <- c('altura_tile_30arc', 'roughness', 'prec', 'treecover_agg', 'sq_1km.1', 'slope')
discontinuity <- 'dist_disc'

# simple example
# test <- RATest::RDperm(data = defo_dist[3][[1]],
#                        W = vars,
#                        z = discontinuity,
#                        cutoff = 0,
#                        n.perm = 1000)
# summary(test)

# Calculate permutation tests
perm_test <- perm_test_list(list_df = list_df ,
                              names = areas,
                              covs = vars,
                              z = discontinuity,
                              n = 20000,
                              c = 0)
perm_test

saveRDS(perm_test, str_c(Sys.getenv("OUTPUT_FOLDER"), "/RD/Models/new_results/perm_test.rds"))

# Calculate descriptive stats for a 5km buffer 
descriptives <- descriptive_stats_buffer(list_df = list_df,
                                           names = areas,
                                           covs = vars,
                                           dist_var = discontinuity,
                                           buffer = 5)

# Merge both tables and concatenate them in a ordered fashion
merge_df <- merge(perm_test, descriptives, by = c('area', 'var'))
joint_test <- anti_join(perm_test, descriptives, by = c('area', 'var')) %>%
  select(area, var, results.T.Sn., results.Pr...z.., results.q) %>%
  mutate(., mean = '',
            sd = '',
            min = '',
            max = '',
            n = ''
         )


# Recode variables and convert to LaTeX

keys_recode = list('Elevation ($m$)', 'Total precipitations (mm3)', 'Roughness index', 'Slope (degrees)', 'Soil quality index', 'Treecover (ha/km^2)', 'Joint Test')
names(keys_recode) <- c(unique(merge_df$var), 'Joint.Test')

merged_perm_descriptives <- rbind(merge_df, joint_test) %>% arrange(.,area) %>%
mutate(., var = dplyr::recode_factor(var, !!!keys_recode)) %>%
mutate_if(c(F, F, F, F, F, T, T, T, T, T), as.numeric) %>%
select(area, var, mean, sd, min, max, results.T.Sn., results.Pr...z.. ,results.q) %>%
  stargazer::stargazer(., summary=F, digits = 4, rownames = F)




