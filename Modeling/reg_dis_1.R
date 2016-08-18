
# Run RD regressions (For parks and territories)

############################

rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(multiwayvcov)
library(stringr)
library(stargazer)
library(rddtools)
library(ggplot2)

#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv") %>% select(-X)
cov <- read.csv("geographic_covariates.csv") %>% select(-X)
clump <- read.csv("clump_id_dataframe_2000.csv") %>% select(ID, clumps)


setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes/Estrategia 1")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

territories_2000 <- list_files[str_detect(list_files, "_2000")] %>%
  .[str_detect(., "terr")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)))])
loss_sum <- dplyr::select(defo, c(ID, loss_sum))

#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., clump, by = "ID", all.x = T) %>%
    mutate(clumps = ifelse(is.na(clumps), 0, 1))
})

defo_dist_terr <- lapply(territories_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist) %>%
    merge(., cov, by = "ID", all.x = T) %>%
    merge(., clump, by = "ID", all.x = T) %>%
    mutate(clumps = ifelse(is.na(clumps), 0, 1))
})


#Regression discontinuity (optimal bandwidth)

#All parks RD estimator
rd_robust_parks <- lapply(defo_dist, function(park){
  rdrobust(
    y = I(park$loss_sum * 100),
    x = park$dist_disc,
    cluster = park$ID,
    all = T
  )
})


rd_robust_terr <- lapply(defo_dist_terr, function(terr){
  rdrobust(
    y = I(terr$loss_sum * 100),
    x = terr$dist_disc,
    cluster = terr$ID,
    all = T
  )
})


setwd("~")
rd_robust_terr <- readRDS("rd_robust_terr.rds")
rd_robust_parks <- readRDS("rd_robust_parks.rds")

#Extract LATE's from list and create a table

rd_robust_park_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_robust_parks)){
  rd_robust_park_table[[i]] <- rd_robust_parks[[i]]$tabl3.str[1, ]
}

rd_robust_park_table <- ldply(rd_robust_park_table) %>%
  t()

rd_robust_terr_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_robust_terr)){
  rd_robust_terr_table[[i]] <- rd_robust_terr[[i]]$tabl3.str[1, ]
}

rd_robust_terr_table <- ldply(rd_robust_terr_table) %>%
  t()

rd_robust_table <- cbind(rd_robust_park_table, rd_robust_terr_table) %>%
  stargazer()


for(i in 1:length(rd_nonpara_table)){
  rd_nonpara_table[, i] <- as.numeric(rd_nonpara_table[, i])
}


bws_list <- list()
for(i in 1:length(rd_robust_parks)){
  bws_list[i] <- rd_robust_parks[[i]]$bws[1, 1]
} %>% plyr::ldply(.)

bws_list_t <- list()
for(i in 1:length(rd_robust_terr)){
  bws_list_t[i] <- rd_robust_terr[[i]]$bws[1, 1]
}

#Graphs for RD (using rdrobust)

setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  pdf(str_c("RD_", type, ".pdf"), height=6, width=12)
  rdplot(
    y = (x$loss_sum) * 100,
    x = (x$dist_disc) / 1000,
    binselect = "es",
    y.lim = c(0, 4),
    title = str_c("Regression discontinuity for", type, sep = " "),
    x.label = "Distance to national park frontier (meters)",
    y.label = "Deforestation (Ha x km2)",
    ci = 95)
  dev.off()
}, x = defo_dist, type = c("all parks", "national parks", "regional parks"))

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



# Regression with optimal bw and heterogeneus effects
het_reg <- lapply(defo_dist, function(x){
  lm(formula = loss_sum ~ treatment + poly(dist_disc/1000, 2) + altura_tile_30arc + roughness + slope, data = x,
     subset = dist <= 5000)
})
het_reg_clust <- lapply(het_reg, cluster.vcov, ~ ID)
mapply(coeftest, x  = het_reg , vcov. = het_reg_clust)

stargazer(het_reg)


het_reg_terr <- lapply(defo_dist_terr, function(x){
  lm(formula = loss_sum * 100 ~ treatment*clumps + poly(dist_disc/1000, 2) + altura_tile_30arc + roughness + slope, data = x,
     subset = dist <= 5000)
})
het_reg_clust_terr <- lapply(het_reg_terr, cluster.vcov, ~ ID)
mapply(coeftest, x  = het_reg , vcov. = het_reg_clust)

stargazer(naive_reg_terr)


het_reg_terr <- lapply(defo_dist_terr, function(x){
  lm(formula = loss_sum * 100 ~ treatment*clumps + poly(dist_disc/1000, 2):clumps + altura_tile_30arc:clumps + roughness:clumps + slope:clumps, data = x,
     subset = dist <= 5000)
})
het_reg_clust_terr <- lapply(het_reg_terr, cluster.vcov, ~ ID)
mapply(coeftest, x  = het_reg , vcov. = het_reg_clust)

stargazer(naive_reg_terr)







#Discontinuity plot (ggplot2)

defo_dist_all <- c(defo_dist[2:3] ,defo_dist_terr) #Collapse all dataframes into one list and remove "all"

l <- lapply(defo_dist_all, function(x){
  mutate(x, bin = cut(x$dist_disc / 1000, breaks = c(-50:50), include.lowest = T)) %>%
    group_by(bin) %>%
    summarize(mean_bin = mean(loss_sum), sd_bin = sd(loss_sum), n = length(ID)) %>%
    .[complete.cases(.),] %>%
    as.data.frame() %>%
    mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
    mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
})



#Individual graphs for all territories (natural parks + territories)
setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  g <- ggplot(x, aes(y = (mean_bin * 100), x = as.numeric(bins), colour = as.factor(treatment))) 
  g <- g + stat_smooth(method = "auto") 
  g <- g + geom_point(colour = "black", size = 1)
  g <- g + scale_y_continuous(limits = c(0, 6))
  g <- g + labs(x = "Distancia (Km.)", y = "Deforestación (Ha x Km2)")
  g <- g + ggtitle(str_c("Discontinuidad\n", "para", type, sep = " "))
  g <- g + guides(colour = FALSE)
  g <- g + theme_bw()
  g
  ggsave(str_c("RDggplot_", type, "strategy1",".pdf"), width=30, height=20, units="cm")
}, x = l, type = c("Áreas protegidas nacionales","Áreas protegidas regionales","Resguardos indígenas", "Comunidades negras"))


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




g <- ggplot(l_all, aes(y = (meanbin * 100), x = as.numeric(bins), colour = as.factor(treatment))) 
g <- g + facet_wrap( ~ type, ncol=2)
g <- g + stat_smooth(method = "auto") 
g <- g + geom_point(colour = "black", size = 1)
g <- g + scale_y_continuous(limits = c(0, 6))
g <- g + labs(x = "Distancia (Km)", y = "Deforestación (Ha x Km2)")
g <- g + guides(colour = FALSE)
g <- g + theme_bw()
g
ggsave("RDggplot_all_strategy1.pdf", width=30, height=20, units="cm")

