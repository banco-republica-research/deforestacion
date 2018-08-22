###############################################################################
####################### PLACEBOS FOR MAIN RESULTS RD ##########################
###############################################################################

rm(list=ls())
library(plyr)
library(dplyr)
library(rdrobust)
library(stargazer)
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)
library(rlang)
library(parallel)
library(forcats)
library(purrr)


# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/rd_functions.R")
source("R/func_tables.R")
source("R/aesthetics.R")
source("modeling/merge_datasets.R")


# Set directories
setwd(Sys.getenv("OUTPUT_FOLDER"))

###############################################################################
########################## PLACEBOS TO ALL AREAS  #############################
###############################################################################

list_df <- c(defo_dist[2:3], defo_dist_terr)
vars <- c('loss_sum', 'coca_agg', 'illegal_mining_EVOA_2014')

if('placebos_all.rds' %in% list.files()){
  print(paste0('Placebos already calculated and stored in ', getwd()))
} else {
  placebos_all <- cross2(list_df, vars) %>% 
    invoke_map("rd_placebos", ., start = -10,  end = 10, step = 0.5)
  saveRDS(placebos_all, 'placebos_all.rds')
}

###############################################################################
########################## PLACEBOS TO DATAFRAME  #############################
###############################################################################

# Define cut-offs to rename names
cut_offs <- seq(-10, 10, 0.5)
names <- c('National', 'Regional', 'Black', 'Indigenous') 

#  Define names for identify df
repeated_names <- names %>% 
  rep(., length(cut_offs)) %>%
  .[order(match(., names))] %>%
  rep(., length(vars))

repeated_vars <- vars %>%
  rep(length(names) * length(cut_offs)) %>%
  .[order(match(., vars))] 


# Convert Placebos to df
placebos_df <- placebos_all %>%
  unlist(recursive = F) %>%  
  lapply(extract_values) %>%
  ldply() %>%
  mutate(cut_offs = rep(cut_offs, length(placebos_all)),
         area = factor(repeated_names),
         area = fct_relevel(area, c('National', 'Regional', 'Indigenous', 'Black')),
         var = repeated_vars,
         var_name = fct_recode(var, Deforestation = 'loss_sum',
                               `Coca Crops` = 'coca_agg',
                               Mining = 'illegal_mining_EVOA_2014')
         ) 

# Correct estimators out of CI's
placebos_df_corr <- placebos_df %>%
  mutate(sanity_coef = ifelse(coef < ci_r & coef > ci_l, TRUE, FALSE),
         new_coef = ifelse(sanity_coef == F, (ci_l + ci_r) / 2, coef))

###############################################################################
################################ PLOTS PLACEBOS ###############################
###############################################################################

df_list_vars <- split(placebos_df_corr, placebos_df_corr$var) 

plts <- lapply(df_list_vars, function(x){
  g <- ggplot(x, aes(x =cut_offs , y =new_coef))
  g <- g + geom_line(size = 1)
  g <- g + geom_ribbon(aes(ymin = ci_l, ymax = ci_r), alpha = 0.2)
  g <- g + facet_wrap(~area, ncol = 1, scales = 'free')
  g <- g + geom_vline(xintercept = 0, linetype = 2) 
  g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
  g <- g + labs(x = 'Cut offs (km)', y = 'Coefficient')
  g <- g + theme_bw()
  g
})

paths <- (str_c('RD/Graphs/RD_placebos_new_', names(plts) ,'.pdf'))
pwalk(list(paths, plts), ggsave, 
      path = getwd(), 
      device = 'pdf',
      width = 30, 
      height = 30, 
      units = 'cm')


