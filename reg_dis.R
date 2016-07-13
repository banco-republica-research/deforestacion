library(rdrobust)
library(rdd)
library(rddtools)
########################## REGRESSION DISCONTINUITY ################################

#Import datasets
setwd("Dropbox/BANREP/Deforestacion/Datos/Dataframes")
defo <- read.csv("dataframe_deforestacion.csv")
dist_2000_all <- readRDS("dist_2000_all.rds")

#Aggregate deforestation (2001 - 2013?)
defo$loss_sum <- rowSums(defo[, c(4:length(names(defo)))])

#Merge data
defo_dist <- merge(defo, dist_2000_all, by.x = "ID", by.y = "ID")
defo_dist$dist_disc <- ifelse(defo_dist$ID %in% unlist(cells_naturalparks), 1, -1) * defo_dist$dist

#Create regional identifier
regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion",
              "Reserva Forestal Protectora Nacional")
defo_dist$regional <- ifelse(defo_dist$DESIG %in% regional, 1 , 0)


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


