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
################################################### SENSIBILITY TESTS ####################################################

#Define bw's: 1 km to 15 km each 500 m. and including optimal bw

bws_optimal_ctrl <- df_optimal_ctrl %>% .[10, ] %>% as.numeric()

distances_bw <- lapply(bws_optimal_ctrl, function(bw){
  dist <- c(seq(1, 15, by = 0.5), bw) %>%
    .[sort.list(.)]
})


list_df <- c(defo_dist[2:3], defo_dist_terr)

#National parks
rd_robust_sensibility_national <- list()
for(i in distances_bw[[1]]){
  rd_robust_sensibility_national[[as.character(i)]] <-
    rdrobust(
      y = list_df[[1]]$loss_sum,
      x = list_df[[1]]$dist_disc,
      covs = cbind(list_df[[1]]$altura_tile_30arc, list_df[[1]]$slope, list_df[[1]]$roughness, list_df[[1]]$prec, 
                   list_df[[1]]$sq_1km.1, list_df[[1]]$treecover_agg, list_df[[1]]$clumps_1),
      vce = "hc1",
      all = T,
      h = i[[1]],
    )
}

#Regional parks
rd_robust_sensibility_regional <- list()
for(i in distances_bw[[2]]){
  rd_robust_sensibility_regional[[as.character(i)]] <-
    rdrobust(
      y = list_df[[2]]$loss_sum,
      x = list_df[[2]]$dist_disc,
      covs = cbind(list_df[[2]]$altura_tile_30arc, list_df[[2]]$slope, list_df[[2]]$roughness, list_df[[2]]$prec, 
                   list_df[[2]]$sq_1km.1, list_df[[2]]$treecover_agg, list_df[[2]]$clumps_1),
      vce = "hc1",
      all = T,
      h = i,
    )
}

#Indigenous
rd_robust_sensibility_indigenous <- list()
for(i in distances_bw[[3]]){
  rd_robust_sensibility_indigenous[[as.character(i)]] <-
    rdrobust(
      y = list_df[[3]]$loss_sum,
      x = list_df[[3]]$dist_disc,
      covs = cbind(list_df[[3]]$altura_tile_30arc, list_df[[3]]$slope, list_df[[3]]$roughness, list_df[[3]]$prec, 
                   list_df[[3]]$sq_1km.1, list_df[[3]]$treecover_agg, list_df[[3]]$clumps_1),
      vce = "hc1",
      all = T,
      h = i,
    )
}

#Black communities
rd_robust_sensibility_black <- list()
for(i in distances_bw[[4]]){
  rd_robust_sensibility_black[[as.character(i)]] <-
    rdrobust(
      y = list_df[[4]]$loss_sum,
      x = list_df[[4]]$dist_disc,
      covs = cbind(list_df[[4]]$altura_tile_30arc, list_df[[4]]$slope, list_df[[4]]$roughness, list_df[[4]]$prec, 
                   list_df[[4]]$sq_1km.1, list_df[[4]]$treecover_agg, list_df[[4]]$clumps_1),
      vce = "hc1",
      all = T,
      h = i,
    )
}

################################################ GRAPHS AND TABLES #####################################################
############################################## RD OBJECT TO DATAFRAME FUNCTION ########################################
rd_to_df <- function(list, name, dist){
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
    mutate(Discontinuidad = dist) %>% mutate_all(funs(as.character)) %>% mutate_all(funs(as.numeric)) %>%
    mutate(type = name) %>% mutate(type = factor(type, levels = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras")))
  return(rd)
}

sens_tests <- list(rd_robust_sensibility_national,
                 rd_robust_sensibility_regional,
                 rd_robust_sensibility_indigenous,
                 rd_robust_sensibility_black)

senst_test_df <- mapply(rd_to_df, list = sens_tests, dist = distances_bw,
                      name = c("Nacionales", "Regionales", "Indígenas", "Comunidades negras"), SIMPLIFY = F) %>%
  ldply() %>% arrange(Discontinuidad) %>% mutate(optimal = ifelse(bw %in% bws_optimal_ctrl, 1, 0))

#Graph LATE for all distances with IC's
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
g <- ggplot(senst_test_df, aes(y = Tratamiento, x = Discontinuidad)) 
g <- g + facet_wrap( ~ type, ncol=1, scales = "fixed")
g <- g + geom_line()
# g <- g + scale_y_continuous(lim = c(-0.12, 0.2))
g <- g + coord_cartesian(xlim = c(0, 15))
g <- g + geom_ribbon(aes(ymin = CI_l, ymax = CI_u), alpha = 0.2)
# g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_vline(data = senst_test_df[senst_test_df$optimal == 1, ], aes(xintercept = bws_optimal_ctrl), colour="red")
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + theme_bw()
g
ggsave("RDggplot_sens_test.pdf", width=30, height=20, units="cm")


############################################### GRAPH AND TABLE FOR PANEL ###############################################
################################################### DISTANCE PLACEBOS ###################################################

setwd("~/Dropbox/BANREP/Deforestacion/Results/Panel/")
list_files <- list.files()
sensi_panel <- list_files[str_detect(list_files, "sensi")] %>%
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
g <- ggplot(sensi_panel, aes(y = Tratamiento, x = Discontinuidad)) 
g <- g + facet_wrap( ~ type, ncol = 1, scales = "fixed")
g <- g + geom_line() 
g <- g + geom_ribbon(aes(ymin = CI_l, ymax = CI_u), alpha = 0.2)
# g <- g + geom_vline(xintercept = 0, linetype = 2) 
g <- g + geom_hline(yintercept = 0, linetype = 2, colour = "grey")
g <- g + coord_cartesian(xlim = c(0, 15))
g <- g + theme_bw()
g
ggsave("RDggplot_sense_panel.pdf", width=30, height=20, units="cm")







