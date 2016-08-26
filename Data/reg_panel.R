
# Create datasets for Panel regressions

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

# X = {clumps, geography, codmun(conflict)}
clumps <- fread("Datos/Clumps/clump_id_dataframe_2000.csv")
clumps <- subset(clumps,,c("ID","clumps"))
geo <- fread(paste0(data,"geographic_covariates.csv"))
geo <- subset(geo,,c( "ID", "altura_tile_30arc", "slope", "roughness", "tri"))
mun <- fread(paste0(data,"colombia_municipios_code_r.csv"))
mun <- subset(mun,,c("ID","layer"))

x <-  merge(clumps,geo, by = "ID", all = TRUE)
x <-  merge(x,mun, by = "ID", all = TRUE)

conflict <- read.dta("Datos/Conflicto/conflicto_pre2000.dta")
x <-  merge(x,conflict, by.x = "layer", by.y = "codmun", all = TRUE)

############################
# Merge distances by type of area to defo and X

# areas <- c("all","national","regional", "terr1", "terr2")
areas <- c("terr1", "terr2")

for(d in c(1:1)) { 
  print(paste0("distance ",d))
  
  for(a in areas) {
    print(paste0("area ",a))
    dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
    defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID)]
    defo_temp <- melt(defo_temp, id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo")
    defo_temp$year <- as.numeric(substr(defo_temp$year, 21,23)) + 1999
    defo_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
    defo_dist <- merge(defo_temp, x, by ="ID", all.x = TRUE)
    defo_dist$clumps[is.na(defo_dist$clumps)] <- 0

    # Export to stata
    eval(parse(text=paste("write.dta(defo_dist,paste0(data,\"Estrategia ",d,"/defo_panel_",a,".dta\"))", sep="")))
  }
}

############################
# Merge defo to distances for each protected area (1-15)

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  
  for(a in 1:15) {
    print(paste0("area ",a))
    ok <- file.exists(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
    if (ok) {
      dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
      defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID)]
      defo_temp <- melt(defo_temp, id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo")
      defo_temp$year <- as.numeric(substr(defo_temp$year, 21,23)) + 1999
      defo_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
      defo_dist <- merge(defo_temp, x, by ="ID", all.x = TRUE)
      defo_dist$clumps[is.na(defo_dist$clumps)] <- 0
      
      # Export to stata
      eval(parse(text=paste("write.dta(defo_dist,paste0(data,\"Estrategia ",d,"/defo_panel_",a,".dta\"))", sep="")))
    }
  }
}


