
# Create datasets for Panel regressions

############################

rm(list=ls())
library(data.table)
library(foreign)
library(plyr)
library(dplyr)
library(magrittr)
library(rlist)
library(stringi)

# mac
<<<<<<< HEAD
setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
# setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Deforestacion/")

=======
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/")
setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Deforestacion/")
>>>>>>> origin/master
data <-"Datos/Dataframes/"

########################################################

# Datasets: 2001-2016  

########################################################

# Deforestation (2001-2016): Expressed in %. loss_year_brick_1km.1 is % without change 
defo <- fread(paste0(data,"dataframe_deforestacion.csv")) %>% 
  select(contains("loss"), ID) %>% select(-c(loss_year_brick_1km.1)) 

# Coca (2001-2016)
coca <- read.csv(paste0(data, "cocasimci.csv")) %>%
  select(contains("coca"), ID) %>% select(-c(coca_1km_brick,coca_geometry,coca_GRI_1K_F)) %>% 
  mutate(ID = as.character(as.numeric(ID)))

# X = {clumps, geography, codmun(conflict)}
geo <- fread(paste0(data,"geographic_covariates_new.csv")) %>% 
<<<<<<< HEAD
  select(ID, altura_tile_30arc, slope, roughness, tri,prec, sq_1km.1, roads, clumps_1, clumps_5k, clumps_10k)
treecover <- fread(paste0(data, "treecover_2000.csv")) %>% dplyr::select(ID, treecover_agg)
mun <- fread(paste0(data,"colombia_municipios_code_r.csv")) %>% 
  select(ID, layer) 
# conflict <- read.dta("Datos/Conflicto/conflicto_pre2000.dta")
x <-  merge(geo,treecover, by = "ID", all = TRUE)
=======
  select(ID, altura_tile_30arc, slope, roughness, tri, prec,sq_1km.1, roads, clumps_1,clumps_5k,clumps_10k)
tree <- fread(paste0(data,"treecover_2000.csv")) %>% select(ID, treecover_agg)
mun <- fread(paste0(data,"colombia_municipios_code_r.csv")) %>% select(ID,layer)
# conflict <- read.dta("Datos/Conflicto/conflicto_pre2000.dta")

x <-  merge(geo,tree, by = "ID", all = TRUE)
>>>>>>> origin/master
x <-  merge(x,mun, by = "ID", all = TRUE)
# x <-  merge(x,conflict, by.x = "layer", by.y = "codmun", all = TRUE)

write.dta(x,paste0(data,"covariates.dta"))

<<<<<<< HEAD
x_hete <- x %>% 
  select(ID,roads, clumps_1, clumps_5k, clumps_10k)
=======
x_hete <- x %>% select(ID,roads,clumps_1,clumps_5k,clumps_10k)
>>>>>>> origin/master

############################
# Merge distances by type of area to defo and X

areas <- c("national","regional", "private", "terr1", "terr2")

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  for(a in areas) {
    print(paste0("area ",a))
    
    dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))

    defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID),] %>%
      melt(., id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo") %>% 
      mutate(year = as.numeric(stri_sub(year, 21,23)) + 1999) %>% 
      mutate(defo = 100 * defo)
    coca_temp <- coca[coca$ID %in% unique(dist_temp$ID),] %>% 
      melt(., id.vars = "ID", measure.vars = 1:16, variable.name = "year", value.name = "coca") %>% 
      mutate(year = as.numeric(stri_sub(year, -2)) + 2000)
    
    dist_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
    dist_temp <- merge(dist_temp, coca_temp,by = c("ID", "year"), all.x = TRUE)
    dist_temp$coca[is.na(dist_temp$coca)] <- 0
    dist_temp <- merge(dist_temp, x_hete, by ="ID", all.x = TRUE)
    dist_temp$clumps_1[is.na(dist_temp$clumps_1)] <- 0
    dist_temp$clumps_5k[is.na(dist_temp$clumps_5k)] <- 0
    dist_temp$clumps_10k[is.na(dist_temp$clumps_10k)] <- 0
    dist_temp$roads[is.na(dist_temp$roads)] <- 0
    
    # Export to stata
    eval(parse(text=paste("write.dta(dist_temp,paste0(data,\"Estrategia ",d,"/defo_panel_",a,".dta\"))", sep="")))
  }
}



############################
# OLD: Merge defo to distances for each protected area (1-15)

# for(d in c(2:2)) { 
#   print(paste0("distance ",d))
#   
#   for(a in 1:15) {
#     print(paste0("area ",a))
#     ok <- file.exists(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
#     if (ok) {
#       dist_temp <- readRDS(paste0(data,"Estrategia ",d,"/dist_panel_",a,".rds"))
# 
#       defo_temp <-  defo[defo$ID %in% unique(dist_temp$ID),] %>%
#         melt(., id.vars = "ID", measure = patterns("^loss_year_brick_1km."), variable.name = "year", value.name = "defo") %>% 
#         mutate(year = as.numeric(stri_sub(year, 21,23)) + 1999)
#       coca_temp <- coca[coca$ID %in% unique(dist_temp$ID),] %>% 
#         melt(., id.vars = "ID", measure.vars = 1:16, variable.name = "year", value.name = "coca") %>% 
#         mutate(year = as.numeric(stri_sub(year, -2)) + 2000)
#       
#       dist_temp <- merge(dist_temp, defo_temp,by = c("ID", "year"))
#       dist_temp <- merge(dist_temp, coca_temp,by = c("ID", "year"), all.x = TRUE)
#       dist_temp$coca[is.na(dist_temp$coca)] <- 0
#       dist_temp <- merge(dist_temp, x_hete, by ="ID", all.x = TRUE)
#       dist_temp$clumps[is.na(dist_temp$clumps)] <- 0
#       dist_temp$roads[is.na(dist_temp$roads)] <- 0
#       
#       # Export to stata
#       eval(parse(text=paste("write.dta(dist_temp,paste0(data,\"Estrategia ",d,"/defo_panel_",a,".dta\"))", sep="")))
#     }
#   }
# }
# 
# 


############################
# Varios 

# Totales por ano
defo_tot <- defo %>% select(-ID) %>% mutate(a=1) %>%
  group_by(a) %>% summarise_all(sum) %>% select(-a) %>% 
  melt(., measure.vars = 1:16, variable.name = "year", value.name = "defo") %>% 
  mutate(year = as.numeric(stri_sub(year, 21,23)) + 1999) %>% 
  mutate(defo = 100*defo)
coca_tot <- coca %>% select(-ID) %>% mutate(a=1) %>%
  group_by(a) %>% summarise_all(mean) %>% select(-a) %>% 
  melt(., measure.vars = 1:16, variable.name = "year", value.name = "coca") %>% 
  mutate(year = as.numeric(stri_sub(year, -2)) + 2000)

out <- merge(defo_tot, coca_tot, by="year")
write.dta(out,paste0(data,"defo_coca.dta"))
