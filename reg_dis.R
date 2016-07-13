
# Create dataframes for regressions

############################

rm(list=ls())
library(data.table)
library(rdrobust)
library(rdd)
library(rddtools)

# Leonardo
# setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
setwd("Dropbox/BANREP/Deforestacion/")

data <-"Datos/Dataframes/"

########################## REGRESSION DISCONTINUITY ################################

#Import datasets
defo <- read.csv(paste0(data,"dataframe_deforestacion.csv"))
dist_2000_all <- readRDS(paste0(data,"dist_2000_all.rds"))
dist_2000_national <- readRDS(paste0(data,"dist_2000_national.rds"))
dist_2000_regional <- readRDS(paste0(data,"dist_2000_regional.rds"))

#Aggregate deforestation (2001 - 2013?)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)))])

#Merge data
defo_dist <- merge(defo, dist_2000_all, by.x = "ID", by.y = "ID")
defo_dist$dist_disc <-  ifelse(defo_dist$treatment, 1, -1) * defo_dist$dist
  
defo_dist_nat <- merge(defo, dist_2000_national, by.x = "ID", by.y = "ID")
defo_dist_nat$dist_disc <- ifelse(defo_dist_nat$treatment, 1, -1) * defo_dist_nat$dist

defo_dist_reg <- merge(defo, dist_2000_regional, by.x = "ID", by.y = "ID")
defo_dist_reg$dist_disc <- ifelse(defo_dist_reg$treatment, 1, -1) * defo_dist_reg$dist

# Naive regression

lm(loss_sum ~ treatment, data = defo_dist)
lm(loss_sum ~ treatment, data = defo_dist_nat)
lm(loss_sum ~ treatment, data = defo_dist_reg)


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
  ci = T,
  subset = defo_dist$regional != 1
)


