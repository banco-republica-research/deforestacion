
# Run Panel regressions

############################

rm(list=ls())
library(data.table)
library(foreign)

# Leonardo
setwd("C:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# Ivan 
# setwd("Dropbox/BANREP/Deforestacion/")

data <-"Datos/Dataframes/"

########################################################

# Datasets: 2001-2012  
# (and export to stata)

########################################################

# Deforestation 
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))
defo <- defo[, (c("loss_year_brick_1km.1","loss_year_brick_1km.14")) := NULL]

# Merge defo to distances by type of area 

areas <- c("all","national","regional")
defo_dist <- list()

for(a in areas) {
  print(paste0("area ",a))
  eval(parse(text=paste("dist_temp <- readRDS(paste0(data,\"dist_panel_",a,".rds\"))", sep="")))
  defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID)]
  defo_temp <- melt(defo_temp, id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo")
  defo_temp$year <- as.numeric(substr(defo_temp$year, 21,23)) + 1999
  defo_dist[[a]] <- merge(dist_temp, defo_temp,by = c("ID", "year"))
  eval(parse(text=paste("write.dta(defo_dist[[",a,"]],paste0(data,\"defo_panel_",a,".dta\"))", sep="")))
}


########################################################

# Regressions  

########################################################
