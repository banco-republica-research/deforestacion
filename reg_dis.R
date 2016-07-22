
# Run RD regressions (All, national and regional)

############################

rm(list=ls())
library(plyr)
library(dplyr)
library(data.table)
library(rdrobust)
library(rdd)
library(rddtools)

#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(., readRDS) %>%
  lapply(., data.frame)

#Aggregate deforestation (2001 - 2012)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)))])
loss_sum <- select(defo, c(ID, loss_sum))

#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(loss_sum, x, by.x = "ID", by.y = "ID") %>%
    mutate(., dist_disc = ifelse(treatment == 1, 1, -1) * dist)
  })


# Naive regression
library(stargazer)
naive_reg <- lapply(defo_dist, function(x){
  lm(formula = loss_sum ~ treatment , data = x)
})
stargazer(naive_reg)

#Regression discontinuity 

#All parks RD estimator
p <- rdrobust(
  y = defo_dist$loss_sum,
  x = defo_dist$dist_disc
)

setwd("~/Dropbox/BANREP/Deforestacion/Results/RD/Graphs/")
mapply(function(x, type){
  pdf(str_c("RD_", type, ".pdf"), height=6, width=12)
  rdplot(
    y = x$loss_sum,
    x = x$dist_disc,
    y.lim = c(0, 0.2),
    title = str_c("Regresion discontinuity for", type, sep = " "),
    x.label = "Distance to national park frontier",
    y.label = "Deforestation (%)")
  dev.off()
}, x = defo_dist, type = c("all parks", "national parks", "regional parks"))


