
# Run RD regressions

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

########################################################

# Datasets  

########################################################

#Aggregate deforestation (2001 - 2012!)
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))
defo[, loss_sum := Reduce(`+`, .SD), .SDcols=c(4:15)][]
defo_merge <- defo[, c(2,17), with = FALSE]

# defo1 <- read.csv(paste0(data,"dataframe_deforestacion.csv"))
# defo1$loss_sum <- rowSums(defo1[, c(4:15)])
  
# Merge defo to distances by type of area 

areas <- c("all","national","regional")
defo_dist <- list()

for(a in areas) {
  print(paste0("area ",a))
  eval(parse(text=paste("dist_temp <- readRDS(paste0(data,\"dist_2000_",a,".rds\"))", sep="")))
  dist_temp <- merge(dist_temp, defo_merge,by.x = "ID", by.y = "ID")
  print(dim(dist_temp))
  dist_temp$dist_disc <-  ifelse(dist_temp$treatment, 1, -1) * dist_temp$dist
  defo_dist[[a]] <- dist_temp
  }


########################################################

# Regressions  

########################################################

# Naive regression

for(a in areas) {
  print(paste0("Area ",a))
  print(lm(loss_sum ~ treatment, data = defo_dist[[a]]))
  }

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


