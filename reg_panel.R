
# Run Panel regressions

############################

rm(list=ls())
library(data.table)
library(rdrobust)
library(rdd)
library(rddtools)

# Leonardo
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
# setwd("Dropbox/BANREP/Deforestacion/")

data <-"Datos/Dataframes/"

########################################################

# Datasets  

########################################################

# Deforestation (2001 - 2012!)
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))

# Merge defo to distances by type of area 

areas <- c("all","national","regional")
defo_dist <- list()

dist_panel <- readRDS(paste0(data,"dist_panel_all.rds"))
defo_dist <-  defo[defo$ID %in% unique(dist_panel$ID)]








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
