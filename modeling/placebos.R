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
 
placebos_all <- cross2(list_df, vars) %>% 
  invoke_map("rd_placebos", ., start = -10,  end = 10, step = 0.5)


saveRDS(placebos_all, 'placebos_all.rds')

###############################################################################
########################## PLACEBOS TO DATAFRAME  #############################
###############################################################################

# Define cut-offs to rename names
cut_offs <- seq(-10, 10, 0.5)
names <- c('National', 'Regional', 'Black', 'Indigenous') 

#  Define names for identify df
repeated_names <- names %>% 
  rep(length(cut_offs)) %>%
  .[order(match(., names))] 

# Convert Placebos to df
placebos_df <- placebos_all %>%
  unlist(recursive = F) %>%  
  lapply(extract_values) %>%
  ldply() %>%
  mutate(cut_offs = rep(cut_offs, length(placebos_all)),
         area = repeated_names,
         area = fct_reorder(area, c('National', 'Regional', 'Indigenous', 'Black')))

# Correct estimators out of CI's
placebos_df_corr <- placebos_df %>%
  mutate(sanity_coef = ifelse(coef < ci_r & coef > ci_l, TRUE, FALSE),
         new_coef = ifelse(sanity_coef == F, (ci_l + ci_r) / 2, coef))

###############################################################################
################################ PLOTS PLACEBOS ###############################
###############################################################################


g <- ggplot(placebos_df_corr, aes(x =cut_offs , y =new_coef))
g <- g + geom_line(size = 1)
g <- g + geom_ribbon(aes(ymin = ci_l, ymax = ci_r), alpha = 0.2)
g <- g + facet_wrap(~area, ncol = 1, scales = 'free')
g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + labs(x = 'Cut offs (km)', y = 'Coefficient')
g <- g + theme_bw()
g
ggsave('RD/Graphs/RD_placebos_new.pdf', width = 30, height = 30, units = 'cm')

################################################ GRAPHS AND TABLES #####################################################
############################################## RD OBJECT TO DATAFRAME FUNCTION ########################################

#Graph LATE for all distances with IC's
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
g <- ggplot(placebos_df, aes(y = Tratamiento, x = Discontinuidad)) 
g <- g + facet_wrap( ~ type, ncol=1, scales = "free")
g <- g + geom_line() 
g <- g + geom_ribbon(aes(ymin = CI_l, ymax = CI_u), alpha = 0.2)
g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + theme_bw()
g
ggsave("RDggplot_placebos.pdf", width=30, height=20, units="cm")



############################################### GRAPH AND TABLE FOR PANEL ###############################################
################################################### DISTANCE PLACEBOS ###################################################

setwd("~/Dropbox/BANREP/Deforestacion/Results/Panel/")
list_files <- list.files()
robust_panel <- list_files[str_detect(list_files, "robust")] %>%
  .[order(.)[c(2, 3, 4, 5, 1)]] %>%
  lapply(function(x){
    read.table(x, row.names = NULL) %>%
      select(c1:c5) %>%
      .[2:dim(.)[1], ] %>%
      rename(Discontinuidad = c1, N = c2, Tratamiento = c3, SE = c4, t = c5) %>%
      mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>%
      mutate(Discontinuidad = Discontinuidad / 1000) %>%
      mutate(CI_u = Tratamiento + (SE * 1.96)) %>%
      mutate(CI_l = Tratamiento - (SE * 1.96))
  }) %>% mapply(function(x, name){
    mutate(x, type = name) %>%
      mutate(type = factor(type, levels = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras", "Sociedad Civil")))
  }, x = ., name = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras", "Sociedad Civil"), SIMPLIFY = F) %>%
  ldply() %>% arrange(Discontinuidad)
    

#Graph LATE for all distances with IC's
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
g <- ggplot(robust_panel, aes(y = Tratamiento, x = Discontinuidad)) 
g <- g + facet_wrap( ~ type, ncol=1, scales = "free")
g <- g + geom_line() 
g <- g + geom_ribbon(aes(ymin = CI_l, ymax = CI_u), alpha = 0.2)
g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + theme_bw()
g
ggsave("RDggplot_placebos_panel.pdf", width=30, height=20, units="cm")



