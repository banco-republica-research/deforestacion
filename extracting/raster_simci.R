###############################################################################
######################### EXTRACT DATA FROM SIMCI RASTERS #####################
###############################################################################
rm(list=ls())
library(stringr)
library(raster)
library(magrittr)
library(rgdal)
library(foreign)

# Load functions in R
setwd("~/deforestacion/")
source("R/process_rasters.R") 

# Set directories
data <- "Deforestacion/Datos/"
setwd("~/Dropbox/BANREP/")

# Load Colombia shapefile to mask/crop rasters
colombia_municipios <- 
  readOGR(dsn = paste0(data, "Geografia"), layer="Municipios") %>%
  spTransform(CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Get deforestation raster for reference 
res <- brick(paste0(data, "HansenProcessed/1.4/loss_year_brick_1km.tif"))

#Load SIMCI processed rasters (from Python script) and make pixels comparable
file_names <- c("coca", "illegal_mining")

simci_rasters <- lapply(file_names, function(x){
  #Open and process rasters (correct zeros)
  proc_raster <- list.files(paste0(data, "SIMCI"), full.names = TRUE) %>%
  .[str_detect(., x)] %>%
    
    lapply(., function(layer){
    x <- raster(layer)
    x[is.na(x[])] <- 0
    print("Zero correction [DONE]")
    return(x)
  }) %>% 
    raster::brick() %>%
    setExtent(., extent(res))
  print(paste0(x, " [PROCESSED TO STACK]"))
  
   #Extract data
  raster_plain_csv <- as.data.frame(proc_raster, xy = TRUE, na.rm = TRUE)
  raster_plain_csv$ID <- row.names(raster_plain_csv)
  write.csv(raster_plain_csv, 
            file =  paste0(data, "Dataframes", "/", x, "_", "simci", "_", "extract", ".csv"))
  writeRaster(proc_raster, 
              filename = paste0(data,"Dataframes", "/", x, "_", "simci", "_", "extract", ".tif"),
              format =  format = "GTiff",
              options = "INTERLEAVE=BAND",
              progress = 'text')
   print(paste0(x, "[EXTRACTED]"))
   return(proc_raster)
})


############################# VERIFICATIONS ###################################

#YOU CAN LOAD COCA SHAPEFILE (RAW DATA) TO VERIFY MAX IN EACH YEAR/LAYER
#HERE 2016 AS AN EXAMPLE

coca <- readOGR(dsn = paste0(data, "SIMCI"), layer = "Coca_01_16_grillas1k")
coca_16 <- raster(paste0(data, "SIMCI", "/", "coca_COCA_C_16.tif"))
identical(cellStats(coca_16, stat = max), max(coca[, "COCA_C_16"]))

#VERIFY RASTER RESOLUTION (SIMCI AND DEFORESTATION) FOR BOTH COCA AND MINING
identical(coordinates(res), coordinates(simci_rasters[[1]]))
identical(coordinates(res), coordinates(simci_rasters[[2]]))

##############################################################################

raster::writeRaster(simci_rasters[[1]], 
                    filename = "Deforestacion/Datos/SIMCI/simci_coca_agg.tif", 
                    format = "GTiff",
                    options = "INTERLEAVE=BAND",
                    progress = 'text')




