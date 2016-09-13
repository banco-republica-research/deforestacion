
# Run RD regressions (For parks and territories)

############################

rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(stringr)
library(stargazer)
library(rddtools)
library(ggplot2)

#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv") %>% dplyr::select(-X)
cov <- read.csv("geographic_covariates.csv") %>% dplyr::select(-X)
clump <- read.csv("clump_id_dataframe_2000.csv") %>% dplyr::select(ID, clumps)

setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 2")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

territories_2000 <- list_files[str_detect(list_files, "_2000")] %>%
  .[str_detect(., "terr")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)) - 1 )])
loss_sum <- dplyr::select(defo, c(ID, loss_sum))

#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., clump, by = "ID", all.x = T) %>%
    mutate(clumps = ifelse(is.na(clumps), 0, 1))
})

defo_dist_terr <- lapply(territories_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., loss_sum = loss_sum * 100) %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    mutate(., dist_disc = dist_disc / 1000) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., clump, by = "ID", all.x = T) %>%
    mutate(clumps = ifelse(is.na(clumps), 0, 1))
})

#Regression discontinuity for fixed bandwidths (5 and 10 km)
list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T,
    h = 10
  )
})


rd_to_df <- function(list, dataframe){
  rd <- lapply(list, "[", "tabl3.str") %>%
    lapply(as.data.frame) %>%
    lapply( "[", 3 , ) %>%
    ldply() %>% mutate(N_l = unlist(lapply(list, "[", "N_l"))) %>%
    mutate(N_r = unlist(lapply(list, "[", "N_r"))) %>%
    mutate(N = N_l + N_r) %>%
    mutate(bws = unlist(lapply(list, function(x) x$bws[1, 1])))
  
  defo_mean <- mapply(function(x, y){
    y %>%
    filter(abs(dist_disc) <= x$bws[1, 1] & treatment == 0) %>% 
    summarize(mean = mean(loss_sum))
  }, x = list , y = dataframe, SIMPLIFY = F) %>% unlist()
  
  df <- rd %>% cbind(., defo_mean) %>% t() %>% 
    as.data.frame() %>% dplyr::rename(Nacionales = V1,
                   Regionales = V2, Resguardos = V3,
                   Comunidades = V4)
  row.names(df) <- c("Tratamiento", "StdErr", "Z", "p", "CI_l", "CI_u", "N_left","N_right", "N", "bws", "Media control")
  return(df)
}


df_five <- rd_to_df(rd_robust_fixed_five, list_df)
df_ten <- rd_to_df(rd_robust_fixed_ten, list_df)


#Regression discontinuity (optimal bandwidth)

rd_robust_parks_2 <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


rd_robust_terr_2 <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = terr$loss_sum,
    x = terr$dist_disc,
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})

rd_optimal <- c(rd_robust_parks_2[2:3], rd_robust_terr_2)
df_optimal <- rd_to_df(rd_optimal, list_df)

#RD with controls for fixed bandwiths (5km and 10km)

list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_ctrl <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$clumps),
    vce = "nn",
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten_ctrl <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$clumps),
    vce = "nn",
    all = T,
    h = 10
  )
})

df_five_ctrl <- rd_to_df(rd_robust_fixed_five_ctrl, list_df)
df_ten_ctrl <- rd_to_df(rd_robust_fixed_ten_ctrl, list_df)

#Save to latex
df_five_final <- cbind(df_five, df_five_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
df_ten_final <- cbind(df_ten, df_ten_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_five_final, df_ten_final, summary = F)



#Regression discontinuity (optimal bandwidth) with controls

rd_robust_parks_2_ctrl <- lapply(defo_dist, function(park){
  rdrobust(
    y = park$loss_sum,
    x = park$dist_disc,
    covs = cbind(park$altura_tile_30arc, park$slope, park$roughness, park$clumps),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})


rd_robust_terr_2_ctrl <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = terr$loss_sum,
    x = terr$dist_disc,
    covs = cbind(terr$altura_tile_30arc, terr$slope, terr$roughness, terr$clumps),
    vce = "nn",
    nnmatch = 3,
    all = T
  )
})

rd_optimal <- c(rd_robust_parks_2_ctrl[2:3], rd_robust_terr_2_ctrl)
df_optimal_ctrl <- rd_to_df(rd_optimal, list_df) 
df_optimal_final <- cbind(df_optimal, df_optimal_ctrl) %>%
  .[c(1, 5, 2, 6, 3, 7, 4, 8)]
stargazer(df_optimal_final, summary = F)





setwd("~/Dropbox/BANREP/Backup Data/")
saveRDS(rd_robust_terr_2, "rd_robust_terr_2.rds")
saveRDS(rd_robust_parks_2, "rd_robust_parks_2.rds")



bws_list <- list()
for(i in 1:length(rd_robust_parks_2)){
  bws_list[i] <- rd_robust_parks_2[[i]]$bws[1, 1]
} %>% plyr::ldply(.)

bws_list_terr_2 <- list()
for(i in 1:length(rd_robust_terr_2)){
  bws_list_terr_2[i] <- rd_robust_terr_2[[i]]$bws[1, 1]
}


#RD with controls for fixed bandwiths (5km and 10km)

list_df <- c(defo_dist[2:3], defo_dist_terr)
rd_robust_fixed_five_ctrl <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$clumps),
    cluster = x$ID,
    all = T,
    h = 5
  )
})

rd_robust_fixed_ten_ctrl <-  lapply(list_df, function(x){
  rdrobust(
    y = x$loss_sum,
    x = x$dist_disc,
    covs = cbind(x$altura_tile_30arc, x$slope, x$roughness, x$clumps),
    cluster = x$ID,
    all = T,
    h = 10
  )
})

#Graphs for RD (using rdrobust)

defo_dist_all <- c(defo_dist[2:3] ,defo_dist_terr) #Collapse all dataframes into one list and remove "all"

setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  pdf(str_c("RD_", type, ".pdf"), height=6, width=12)
  rdplot(
    y = x$loss_sum,
    x = x$dist_disc,
    kernel = "triangular",
    shade = TRUE,
    binselect = "es",
    y.lim = c(0, 0.3),
    x.lim = c(-20, 20),
    title = str_c("Regression discontinuity for", type, sep = " "),
    x.label = "Distance to national park frontier (meters)",
    y.label = "Deforestation (Ha x km2)",
    ci = 95)
  dev.off()
}, x = defo_dist, type = c("national parks", "regional parks", "resguardos", "blacks"))

setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  pdf(str_c("RD_", type, ".pdf"), height=6, width=12)
  rdplot(
    y = (x$loss_sum) * 100,
    x = (x$dist_disc) / 1000,
    binselect = "es",
    kernel = "triangular",
    y.lim = c(0, 4),
    x.lim = c(-50, 50),
    title = str_c("Regression discontinuity for", type, sep = " "),
    x.label = "Distance to territory frontier (meters)",
    y.label = "Deforestation (Ha x km2)",
    ci = 95)
  dev.off()
}, x = defo_dist_terr, type = c("resguardos", "black territories"))



#Discontinuity plot (ggplot2)

defo_dist_all <- c(defo_dist[2:3], defo_dist_terr) #Collapse all dataframes into one list and remove "all"

l <- lapply(defo_dist_all, function(x){
  mutate(x, bin = cut(x$dist_disc, breaks = c(-50:50), include.lowest = T)) %>%
    group_by(bin) %>%
    summarize(meanbin = mean(loss_sum), sdbin = sd(loss_sum), n = length(ID)) %>%
    .[complete.cases(.),] %>%
    as.data.frame() %>%
    mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
    mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
})



#Individual graphs for all territories (natural parks + territories)
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  g <- ggplot(x, aes(y = (meanbin), x = as.numeric(bins), colour = as.factor(treatment))) 
  g <- g + stat_smooth(method = "auto") 
  g <- g + geom_point(colour = "black", size = 1)
  g <- g + labs(x = "Distancia (Km.)", y = "Deforestación (Ha x Km2)")
  g <- g + scale_x_continuous(limits = c(-20, 20))
  g <- g + scale_y_continuous(limits = c(0, 0.3))
  g <- g + ggtitle(str_c("Discontinuidad\n", "para", type, sep = " "))
  g <- g + guides(colour = FALSE)
  g <- g + theme_bw()
  g
  ggsave(str_c("RDggplot_", type, "strategy2",".pdf"), width=30, height=20, units="cm")
}, x = l, type = c("Áreas protegidas nacionales","Áreas protegidas regionales","Resguardos indígenas", "Comunidades negras"))

#Facet graph
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
library(tidyr)
library(locfit)
l_all <- data.frame(l) %>%
  select(-bin.1, -bin.2, -bin.3, -bins.1, -bins.2, -bins.3, -treatment.1, -treatment.2, -treatment.3) %>%
  gather(key, value, -bins, -bin, -treatment) %>%
  separate(key, c("variable", "type")) %>%
  mutate(type = ifelse(is.na(type), 0, type)) %>%
  spread(variable, value) %>%
  mutate(type = as.factor(type)) %>% mutate(type = mapvalues(type, from = c(0, 1, 2, 3), 
                                                             to = c("Áreas protegidas nacionales",
                                                                    "Áreas protegidas regionales",
                                                                    "Resguardos indígenas", 
                                                                    "Comunidades negras")))




g <- ggplot(l_all, aes(y = meanbin, x = as.numeric(bins), colour = as.factor(treatment))) 
g <- g + facet_wrap( ~ type, ncol=2)
g <- g + stat_smooth(method = "loess") 
g <- g + geom_point(colour = "black", size = 1)
g <- g + scale_x_continuous(limits = c(-20, 20))
g <- g + scale_y_continuous(limits = c(0, 0.3))
g <- g + labs(x = "Distancia (Km)", y = "Deforestación (Ha x Km2)")
g <- g + guides(colour = FALSE)
g <- g + theme_bw()
g
ggsave("RDggplot_all_strategy1.pdf", width=30, height=20, units="cm")


