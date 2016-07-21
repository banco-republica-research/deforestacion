library(rdrobust)
library(rdd)
library(rddtools)
########################## REGRESSION DISCONTINUITY ################################

#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(., readRDS) %>%
  lapply(., data.frame)


#Aggregate deforestation (2001 - 2013?)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)))])

#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(defo, x, by.x = "ID", by.y = "ID")
  })

lapply(defo_dist, function(x){
 x$dist_disc <- ifelse(x$treatment ==1, 1, -1) * defo_dist$dist
})

defo_dist[[1]]$dist_disc <- ifelse(defo_dist[[1]]$treatment ==1, 1, -1) * defo_dist[[1]]$dist

#Regression discontinuity 

#All parks RD estimator
p <- rdrobust(
  y = defo_dist$loss_sum,
  x = defo_dist$dist_disc
)

rdplot(
  y = defo_dist$loss_sum,
  x = defo_dist$dist_disc,
  binselect = "es",
  y.lim = c(0, 0.2),
  ci = T
)


