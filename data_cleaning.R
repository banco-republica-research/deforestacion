#Clenaing treatments and controls
setwd("/Users/Ivan/Dropbox/BANREP/Deforestacion/Datos/Dataframes")
list_files <- list.files()
rds_2012 <- list_files[str_detect(list_files, "dist_2012")] %>%
  lapply(readRDS) %>%
  lapply(data.frame)
