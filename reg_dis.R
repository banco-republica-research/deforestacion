
# Run RD regressions

############################

rm(list=ls())
library(data.table)
library(rdrobust)
library(rdd)
library(rddtools)

<<<<<<< HEAD
<<<<<<< HEAD
#Import datasets
setwd("~/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv")
list_files <- list.files()
rds_2000 <- list_files[str_detect(list_files, "dist_2000")][c(1:3)] %>%
  lapply(., readRDS) %>%
  lapply(., data.frame)

=======
=======
>>>>>>> b020bc6cd94d6db8f9f478306548f010a485f8b9
# Leonardo
# setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
setwd("Dropbox/BANREP/Deforestacion/")

data <-"Datos/Dataframes/"

########################################################

# Datasets  

########################################################
<<<<<<< HEAD

#Aggregate deforestation (2001 - 2012!)
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))
defo[, loss_sum := Reduce(`+`, .SD), .SDcols=c(4:15)][]
defo_merge <- defo[, c(2,17), with = FALSE]

# Merge defo to distances by type of area 
areas <- c("all","national","regional")
defo_dist <- list()

for(a in areas) {
  print(paste0("area ",a))
  dist_temp <- readRDS(paste0(data,"dist_2000_",a,".rds"))
  dist_temp <- merge(dist_temp, defo_merge,by.x = "ID", by.y = "ID")
  print(dim(dist_temp))
  dist_temp$dist_disc <-  ifelse(dist_temp$treatment, 1, -1) * dist_temp$dist
  defo_dist[[a]] <- dist_temp
  }


########################################################
>>>>>>> b020bc6cd94d6db8f9f478306548f010a485f8b9

# Regressions  

<<<<<<< HEAD
#Merge data
defo_dist <- lapply(rds_2000, function(x){
  merge(defo, x, by.x = "ID", by.y = "ID")
  })

lapply(defo_dist, function(x){
 x$dist_disc <- ifelse(x$treatment ==1, 1, -1) * defo_dist$dist
})

defo_dist[[1]]$dist_disc <- ifelse(defo_dist[[1]]$treatment ==1, 1, -1) * defo_dist[[1]]$dist
=======
=======

#Aggregate deforestation (2001 - 2012!)
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))
defo[, loss_sum := Reduce(`+`, .SD), .SDcols=c(4:15)][]
defo_merge <- defo[, c(2,17), with = FALSE]

# Merge defo to distances by type of area 
areas <- c("all","national","regional")
defo_dist <- list()

for(a in areas) {
  print(paste0("area ",a))
  dist_temp <- readRDS(paste0(data,"dist_2000_",a,".rds"))
  dist_temp <- merge(dist_temp, defo_merge,by.x = "ID", by.y = "ID")
  print(dim(dist_temp))
  dist_temp$dist_disc <-  ifelse(dist_temp$treatment, 1, -1) * dist_temp$dist
  defo_dist[[a]] <- dist_temp
  }


########################################################

# Regressions  

>>>>>>> b020bc6cd94d6db8f9f478306548f010a485f8b9
########################################################

# Naive regression

for(a in areas) {
  print(paste0("Area ",a))
  print(lm(loss_sum ~ treatment, data = defo_dist[[a]]))
  }
<<<<<<< HEAD
>>>>>>> b020bc6cd94d6db8f9f478306548f010a485f8b9
=======
>>>>>>> b020bc6cd94d6db8f9f478306548f010a485f8b9

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


