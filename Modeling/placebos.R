rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(stringr)
library(stargazer)
library(foreign)
library(rddtools)
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)

######################################################## ROBUSTNESS ######################################################
####################################################### PLACEBO TESTS ####################################################
list_df <- c(defo_dist[2:3], defo_dist_terr)

#Natural parks
rd_robust_placebo_national <- list()
for(i in seq(-10, 10, by = .5)){
  rd_robust_placebo_national[[as.character(i + 11)]] <-
    rdrobust(
      y = list_df[[1]]$loss_sum,
      x = list_df[[1]]$dist_disc,
      covs = cbind(list_df[[1]]$altura_tile_30arc, list_df[[1]]$slope, list_df[[1]]$roughness, list_df[[1]]$prec, 
                   list_df[[1]]$sq_1km.1, list_df[[1]]$treecover_agg, list_df[[1]]$clumps_1),
      vce = "nn",
      all = T,
      h = 5,
      c = i
    )
}

#Regional parks
rd_robust_placebo_regional <- list()
for(i in seq(from = -10, to = 10, by = 0.5)){
  rd_robust_placebo_regional[[as.character(i + 11)]] <-
    rdrobust(
      y = list_df[[2]]$loss_sum,
      x = list_df[[2]]$dist_disc,
      covs = cbind(list_df[[2]]$altura_tile_30arc, list_df[[2]]$slope, list_df[[2]]$roughness, list_df[[2]]$prec, 
                   list_df[[2]]$sq_1km.1, list_df[[2]]$treecover_agg, list_df[[2]]$clumps_1),
      vce = "nn",
      all = T,
      h = 5,
      c = i
    )
}


#Indigenous
rd_robust_placebo_indigenous <- list()
for(i in seq(from = -10, to = 10, by = 0.5)){
  rd_robust_placebo_indigenous[[as.character(i + 11)]] <-
    rdrobust(
      y = list_df[[3]]$loss_sum,
      x = list_df[[3]]$dist_disc,
      covs = cbind(list_df[[3]]$altura_tile_30arc, list_df[[3]]$slope, list_df[[3]]$roughness, list_df[[3]]$prec, 
                   list_df[[3]]$sq_1km.1, list_df[[3]]$treecover_agg, list_df[[3]]$clumps_1),
      vce = "nn",
      all = T,
      h = 5,
      c = i
    )
  pbPost("note", "Indígenas", str_c("rep", i))
}

#Black communities
rd_robust_placebo_black <- list()
for(i in seq(from = -10, to = 10, by = 0.5)){
  rd_robust_placebo_black[[as.character(i + 11)]] <-
    rdrobust(
      y = list_df[[4]]$loss_sum,
      x = list_df[[4]]$dist_disc,
      covs = cbind(list_df[[4]]$altura_tile_30arc, list_df[[4]]$slope, list_df[[4]]$roughness, list_df[[4]]$prec, 
                   list_df[[4]]$sq_1km.1, list_df[[4]]$treecover_agg, list_df[[4]]$clumps_1),
      vce = "nn",
      all = T,
      h = 5,
      c = i
    )
}



################################################ GRAPHS AND TABLES #####################################################
############################################## RD OBJECT TO DATAFRAME FUNCTION ########################################
rd_to_df <- function(list, name){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 1 , ) %>% 
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1]))) %>%
    as.data.frame() %>%
    rename(Tratamiento = tabl3.str.Coef, SE = tabl3.str.Std..Err., z = tabl3.str.z, p_value = tabl3.str.P..z., CI_l = tabl3.str.CI.Lower, 
           CI_u = tabl3.str.CI.Upper, N_left = N_l, N_right = N_r, N = N, bw = bws ) %>%
    mutate(Discontinuidad = seq(from = -10, to = 10, by = 0.5)) %>% mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>%
    mutate(type = name) %>% mutate(type = factor(type, levels = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras")))
  return(rd)
}


placebos <- list(rd_robust_placebo_national,
                 rd_robust_placebo_regional,
                 rd_robust_placebo_indigenous,
                 rd_robust_placebo_black)

placebos_df <- mapply(rd_to_df, list = placebos, 
                      name = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras"), SIMPLIFY = F) %>%
  ldply() %>% arrange(Discontinuidad)

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



