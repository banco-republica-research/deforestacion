
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

########################################################

# Deforestation (2001-2012) 
defo <- fread(paste0(data,"dataframe_deforestacion.csv"))
defo <- defo[, (c("loss_year_brick_1km.1","loss_year_brick_1km.14")) := NULL]

# X = {clumps, roads, rivers}
lights <- fread("Datos/Clumps/clump_id_dataframe.csv")
lights <- subset(lights,,c("ID","clumps"))

# Merge distances by type of area to defo and X

areas <- c("all","national","regional", "terr1", "terr2")
defo_dist <- list()

for(a in areas) {
  print(paste0("area ",a))
  dist_temp <- readRDS(paste0(data,"dist_panel_",a,".rds"))
  defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID)]
  defo_temp <- melt(defo_temp, id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo")
  defo_temp$year <- as.numeric(substr(defo_temp$year, 21,23)) + 1999
  defo_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
  defo_dist[[a]] <- merge(defo_temp, lights, by ="ID", all.x = TRUE)
  defo_dist[[a]]$clumps[is.na(defo_dist[[a]]$clumps)] <- 0
  
  # Export to stata
  eval(parse(text=paste("write.dta(defo_dist[[\"",a,"\"]],paste0(data,\"defo_panel_",a,".dta\"))", sep="")))
}

# Merge defo to distances for each park area (1-16)

defo_dist <- list()

for(a in 1:16) {
  print(paste0("area ",a))
  ok <- file.exists(paste0(data,"dist_panel_",a,".rds"))
  if (ok) {
    dist_temp <- readRDS(paste0(data,"dist_panel_",a,".rds"))
    defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID)]
    defo_temp <- melt(defo_temp, id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo")
    defo_temp$year <- as.numeric(substr(defo_temp$year, 21,23)) + 1999
    defo_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
    defo_dist[[a]] <- merge(defo_temp, lights, by ="ID", all.x = TRUE)
    defo_dist[[a]]$clumps[is.na(defo_dist[[a]]$clumps)] <- 0

    # Export to stata
    eval(parse(text=paste("write.dta(defo_dist[[",a,"]],paste0(data,\"defo_panel_",a,".dta\"))", sep="")))
  }
}




########################################################

# Regressions  

########################################################
