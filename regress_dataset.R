
# Regressions dataframes

############################

# library
rm(list=ls())
library(data.table)

# Working directory
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")

########################################################

# Read data

defo <- fread("Datos/Dataframes/dataframe_deforestacion.csv")
dist <- fread("Datos/Dataframes/distancia_dataframe.csv")
summary(dist)

