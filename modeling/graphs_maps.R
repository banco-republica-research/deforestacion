##############################################################################
####################### GRAPHS AND MAPS DOCUEMNT #############################
##############################################################################

rm(list=ls())
library(rgeos)
library(dplyr)
library(forcats)
library(tidyr)
library(rgdal)
library(forcats)
library(xtable)
library(ggplot2)
library(ggsn)
library(stringr)
library(raster)
library(parallel)

# Load functions in R
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/process_rasters.R")
source("R/clean_polygons.R")
source("R/calculate_distances.R")
source("cleaning/colombia_continental.R")


# Set directories
setwd(Sys.getenv("DATA_FOLDER"))

#Get deforestation raster for reference: deforestation
res <- brick("HansenProcessed/1.4/loss_year_brick_1km.tif")


##############################################################################
#################### SET UP DATA SETS AND RAW DATA ###########################
##############################################################################

# Open shapefiles
natural_parks <- readOGR(dsn = "UNEP/WDPA_June2016_COL-shapefile", 
                         layer = "WDPA_June2016_COL-shapefile-polygons" ,
                         stringsAsFactors = F) 


# Project to meters 
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) 

# Remove parks outside of the mainland (i.e. Malpelo and Providencia)
natural_parks <- list(natural_parks, natural_parks_proj) %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park")), ] 
  })

# Load territories (blakc & indigenous) datasets (shapefiles) 
black_territories <- readOGR(dsn = "Comunidades", 
                             layer="Tierras de Comunidades Negras (2015)", 
                             stringsAsFactors = F)
indigenous_territories <- readOGR(dsn = "Resguardos", 
                                  layer="Resguardos Indigenas (2015)" , 
                                  stringsAsFactors = F) 

# Change column names to make life easiser (this must have a dplyr way)
colnames(indigenous_territories@data)[8] <- "RESOLUCION"


##############################################################################
######################## FIGURE 2: AREA TIMELINE  ############################
##############################################################################

##############################################################################
############## TOTAL AREA PER YEAR (PARKS & TERRITORIES) #####################
########################## HECTARES/KM2 ######################################
##############################################################################

# Aggregate area data per year for territories 

territories <- 
  list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+init=epsg:3857")) %>%
  lapply(., function(x){
  
    mutate(x@data, 
           STATUS_YR = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", ""),
           area = gArea(x, byid = T) / 1e6) %>%
      group_by(STATUS_YR) %>%
      summarize(total_area = sum(area))
}) %>%
  Reduce(function(...) merge(..., by = "STATUS_YR", all = T, suffixes = c("_black", "_indigenous")), .) %>%
  gather(., type, total_area, total_area_black, total_area_indigenous) %>%
  mutate(., type = as.factor(type)) %>% mutate(., type = fct_recode(type, "Black Communities" = "total_area_black", 
                                                                   "Indigenous Reserves" = "total_area_indigenous"),
                                               STATUS_YR = as.integer(STATUS_YR))
  

# Aggregate area data for natural parks  
regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion")
private <- c("Reserva Natural de la Sociedad Civil")


parks <- natural_parks[[2]]@data %>%
  mutate(area = gArea(natural_parks[[2]], byid = T) / 1e6) %>%
  mutate(type = ifelse(DESIG %in% regional, 'Regional', 
                       ifelse(DESIG %in% private, 'Private', 'National'))) %>%
  group_by(STATUS_YR, type) %>%
  summarize(total_area = sum(area))

#Merge all

all <- rbind(as.data.frame(parks), territories) %>%
  mutate(total_area = ifelse(is.na(total_area), 0, total_area)) %>%
  mutate(cumsum = ave(total_area, type, FUN = cumsum)) %>%
  mutate(cumsum = round(cumsum, 4)) %>%
  mutate(STATUS_YR = as.numeric(STATUS_YR)) %>%
  arrange(STATUS_YR)

# Get graph

setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot(all, aes(y = cumsum / 1000, x = STATUS_YR, colour = type))
g <- g + geom_line(size = 1)
g <- g + labs(x="Year", y = expression(Area~(1000~km^{2})))
g <- g + scale_colour_discrete(name = "Type of Protected Area") 
g <- g + scale_x_continuous(limits = c(1940, 2016), breaks = seq(1940, 2016, by = 5))
g <- g + geom_vline(xintercept = 2000, colour="gray", linetype = "longdash")
g <- g + theme_bw()
g <- g + theme(legend.position = c(.15, .85))
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
g
ggsave(str_c("total_areas_geomarea.pdf"), width=30, height=20, units="cm")


##############################################################################
##################### TABLE 1: ANNUAL DATA PER AREA ##########################
##############################################################################

##############################################################################
################# TOTAL DEFORESTATION (PARKS & TERRITORIES) ##################
################################ FROM RASTERS ################################
##############################################################################

list_areas <- c(natural_parks[[1]], indigenous_territories, black_territories)

get_raster_local_cals <- function(shape, raster, var_name, ...){
  shape_meters <- shape %>% 
    spTransform(CRS=CRS("+init=epsg:3857")) 
  
  # Make calculations and return sp object
  beginCluster(detectCores() - 1)
  calc_polygons_vals <- raster::extract(x = raster,
                                   y = shape,
                                   fun = sum, 
                                   na.rm = T,
                                   df = T,
                                   sp = T)
  
  endCluster()
  
  
  
  # Retrive df and change names to a normal ones
  new_names <- str_c(var_name, c('baseline', c(2001:2016)), sep = '_')
  old_names <- dplyr::select(calc_polygons_vals, starts_with('loss')) %>% colnames()
  
  # Totals
  total <- calc_polygons_vals %>% 
    data.table::setnames(., new = new_names, 
                            old= old_names) %>%
    mutate(., area_km2 = gArea(shape_meters, byid = T) /1e6)
}


# Get individual year deforestation
total_deforestation_polygon <- lapply(list_areas, get_raster_local_cals,
                              raster = res, 
                              var_name = 'defo')


saveRDS(total_deforestation_polygon, 'deforestation_area_polygon.rds')

###############################################################################
################################# OPTIONAL ####################################
###############################################################################

# Merge with shapefile data (in case sp = F is activated in the func)
total_defo_polygon_sp_data <- mapply(function(shape, extracted){
  shape_data <- shape@data
  shape_data_id <- shape_data %>% 
    mutate(ID = as.numeric(row.names(extracted)))
  
  if(dim(shape_data_id)[1] == dim(extracted)[1]){
    print('Good!')
  }
  
  merge_data <- merge(shape_data_id, extracted, by='ID')
}, shape = list_areas, extracted = total_deforestation_polygon)

###############################################################################

# Aggregate area data for natural parks  
regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion")
private <- c("Reserva Natural de la Sociedad Civil")

# Sum and aggreagate by type of area for national parks
total_deforestation_parks <- total_defo_polygon_sp_data[[1]] %>%
  mutate(type = ifelse(DESIG %in% regional, 'Regional', 
                       ifelse(DESIG %in% private, 'Private', 'National'))) %>%
  dplyr::select(., type, GIS_AREA, area_km2, starts_with('defo'), -defo_baseline) %>% 
  group_by(type) %>%
  summarize_all(funs(sum)) %>%
  mutate(total_defo = rowSums(.[4:length(.)]),
         defo_rate = total_defo/area_km2,
         defo_rate_area_year = defo_rate/16,
         total_defo_year = total_defo/16) %>%
  dplyr::select(type, total_defo_year, defo_rate_area_year)
  

# Sum and aggreagte by territory (indigenous and black communities)
total_deforestation_terr <- lapply(total_defo_polygon_sp_data[3:4], function(x){
  x %>%
  dplyr::select(., area_km2, starts_with('defo'), -defo_baseline) %>%
    summarize_all(funs(sum)) %>%
    mutate(total_defo = rowSums(.[2:length(.)]),
           defo_rate = total_defo/area_km2,
           defo_rate_area_year = defo_rate/16,
           total_defo_year = total_defo/16) %>%
    dplyr::select(total_defo_year, defo_rate_area_year)
})
    
##############################################################################
############# TOTAL COCA AND GOLD MINING (PARKS & TERRITORIES) ###############
##############################################################################

##############################################################################
##############################    COCA      ##################################
##############################################################################


# Get updated distances (remember this databases go until 2000)

get_raster_local_cals <- function(shape, raster, var_name, ...){
  shape_meters <- shape %>% 
    spTransform(CRS=CRS("+init=epsg:3857")) 
  
  # Make calculations and return sp object
  beginCluster(detectCores() - 1)
  calc_polygons_vals <- raster::extract(x = raster,
                                        y = shape,
                                        fun = sum, 
                                        na.rm = T,
                                        df = T,
                                        sp = T)
  
  endCluster()
  
  # Totals
  total <- calc_polygons_vals@data %>%
    mutate(., area_km2 = gArea(shape_meters, byid = T) /1e6)
}

# Get coca data
res <- brick('SIMCI/simci_coca_agg.tif')

total_coca_polygon <- lapply(list_areas, get_raster_local_cals,
                                      raster = res)
saveRDS(total_coca_polygon, 'coca_area_polygon.rds')

# Get gold data
res <- brick('SIMCI/illegal_mining_EVOA_2014.tif')

total_gold_polygon <- lapply(list_areas, get_raster_local_cals,
                             raster = res)
saveRDS(total_gold_polygon, 'gold_area_polygon.rds')

# Sum and aggreagate by type of area for national parks
total_coca_parks <- total_coca_polygon[[1]] %>%
  mutate(type = ifelse(DESIG %in% regional, 'Regional', 
                       ifelse(DESIG %in% private, 'Private', 'National'))) %>%
  dplyr::select(., type, GIS_AREA, area_km2, 
                num_range('simci_coca_agg.', 1:16, width = 1)) %>% 
  group_by(type) %>%
  summarize_all(funs(sum)) %>%
  mutate(total_coca = rowMeans(.[4:length(.)])/100,
         coca_rate = total_coca/area_km2,
         coca_rate_area_year = (coca_rate/16) * 100,
         total_coca_year = total_coca/16) %>%
  dplyr::select(type, total_coca_year, coca_rate_area_year)


# Sum and aggreagte by territory (indigenous and black communities)
total_coca_terr <- lapply(total_coca_polygon[3:4], function(x){
  x %>%
    dplyr::select(., area_km2, 
                  num_range('simci_coca_agg.', 1:16, width = 1)) %>%
    summarize_all(funs(sum)) %>%
    mutate(total_coca = rowMeans(.[2:length(.)])/100,
           coca_rate = total_coca/area_km2,
           coca_rate_area_year = coca_rate/16,
           total_coca_year = total_coca/16) %>%
    dplyr::select(total_coca_year, coca_rate_area_year)
}) 


##############################################################################
##############################    GOLD      ##################################
##############################################################################

# Get coca data
res <- brick('SIMCI/illegal_mining_EVOA_2014.tif')

total_gold_polygon <- lapply(list_areas, get_raster_local_cals,
                             raster = res)
saveRDS(total_gold_polygon, 'gold_area_polygon.rds')

# Sum and aggreagate by type of area for national parks
total_gold_parks <- total_gold_polygon[[1]] %>%
  mutate(type = ifelse(DESIG %in% regional, 'Regional', 
                       ifelse(DESIG %in% private, 'Private', 'National'))) %>%
  dplyr::select(., type, GIS_AREA, area_km2, illegal_mining_EVOA_2014) %>% 
  group_by(type) %>%
  summarize_all(funs(sum(., na.rm=T))) %>%
  mutate(total_gold = illegal_mining_EVOA_2014/100,
         gold_rate = total_gold/area_km2) %>%
  dplyr::select(type, total_gold, gold_rate) %>% 
  data.frame()


# Sum and aggreagte by territory (indigenous and black communities)
total_gold_terr <- lapply(total_gold_polygon[2:3], function(x){
  x %>%
    dplyr::select(., area_km2, illegal_mining_EVOA_2014) %>%
    summarize_all(funs(sum(., na.rm = T))) %>%
    mutate(total_gold = illegal_mining_EVOA_2014/100,
           gold_rate = total_gold/area_km2) %>%
    dplyr::select(total_gold, gold_rate) %>%
    data.frame()
}) 


setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot(defo_year_group[[1]], aes(y = defo_year, x =  as.numeric(year)))
g <- g + geom_line(data = defo_year_group[[2]], aes(color = "All parks"), size = 1)
g <- g + geom_line(data = defo_year_group[[3]], aes(color = "National"), size = 1)
g <- g + geom_line(data = defo_year_group[[4]], aes(color = "Regional"), size = 1)
g <- g + geom_line(data = defo_year_group[[5]], aes(color = "Resguardos"), size = 1)
g <- g + geom_line(data = defo_year_group[[6]], aes(color = "Comunitary Lands"), size = 1)
g <- g + scale_x_continuous(breaks = c(2001:2012))
g <- g + scale_colour_discrete(name = "Type of territory")
g <- g + labs(x="Year", y= "Deforested area (Ha x Km2)", title = "Deforestation (2001 - 2012)") 
g <- g + theme_bw()
g
ggsave(str_c("total_deforestation_group.pdf"), width=30, height=20, units="cm")

setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot(defo_year_group[[1]], aes(y = defo_year, x =  as.numeric(year)))
g <- g + geom_line(color = "#839D57", size = 1)
g <- g + labs(x="Year", y= "Deforested area (Ha x Km2)", title = "Deforestation (2001 - 2012)") 
g <- g + scale_x_continuous(breaks = c(2001:2012))
g <- g + theme_bw()
ggsave(str_c("total_deforestation.pdf"), width=30, height=20, units="cm")


# Table deforestation by park type
setwd("~/Dropbox/BANREP/Deforestacion/Datos/HansenProcessed/")
res <- brick("loss_year_brick_1km.tif")

#Get defo by polygon
defo_type <- raster::extract(res, natural_parks[[1]], fun = sum, na.rm = T, df = T) %>%
  defo_type_tot <- defo_type %>%
  mutate(., loss_sum = rowSums(defo_type[, c(4:length(names(defo_type)) - 1 )])) %>%
  dplyr::select(., c(ID, loss_sum)) %>% mutate(.,loss_sum = (loss_sum * 100) / 12)

cells_natural <- cellFromPolygon(res, natural_parks[[1]]) %>%
  sapply(length)

#Get area
parks <- natural_parks[[2]]@data %>%
  mutate(area = gArea(natural_parks[[2]], byid = T) / 1e6) %>%
  mutate(ID = c(1:603)) %>%
  merge(defo_type_tot, by = "ID") %>%
  mutate(area_pixel = cells_natural) %>%
  filter(., STATUS_YR < 2012) %>%
  mutate(., regional = ifelse(DESIG %in% regional, 1, 0)) %>%
  mutate(regional = as.factor(regional)) %>% mutate(.,type = fct_recode(regional, Regional = "1", Nacional = "0")) %>%
  group_by(DESIG, type) %>%
  summarize(defo_total = sum(loss_sum) / 100, 
            area_total = sum(area),
            area_total_pixel = sum(area_pixel)) %>%
  mutate(defo_total_area = defo_total / area_total) %>%
  mutate(defo_total_area_pixel = (defo_total / area_total_pixel) * 100) %>%
  .[c(2, 1, 3, 6, 7, 5, 4)] %>%
  ddply(., c("type")) 

#Create LaTeX table
xtable(parks[, c(1, 2, 3, 5)], auto = T)

#Same for territories

territories_area <- 
  list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+init=epsg:3857")) %>%
  lapply(., function(x){
    mutate(x@data, STATUS_YR = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", "")
           , area = gArea(x, byid = T) / 1e6)
  })


territories <- list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

defo_terr <- lapply(territories, function(x){
  raster::extract(res, x, fun = sum, na.rm = T, df = T)
  })

defo_terr_tot <- defo_terr %>%
  lapply(function(x){
    mutate(x, loss_sum = rowSums(x[, c(4, length(names(x)) - 1 )])) %>%
      select(c(ID, loss_sum)) %>%
      mutate(loss_sum = (loss_sum * 100) / 12)
  })
  

cells_territories <- lapply(territories, function(x){
  cellFromPolygon(res, x) %>%
  sapply(length)
})


territories_df <- mapply(function(x, y , z){
    x %>%
    mutate(ID = c(1:dim(x)[1])) %>%
    merge(y, by = "ID") %>%
    mutate(area_pixel = z) %>%
    filter(., STATUS_YR < 2012) %>%
    group_by() %>%
    summarize(defo_total = sum(loss_sum) / 100, 
              area_total = sum(area),
              area_total_pixel = sum(area_pixel)) %>%
     mutate(defo_total_area = defo_total / area_total) %>%
     mutate(defo_total_area_pixel = (defo_total / area_total_pixel) * 100)
    # .[c(2, 1, 3, 6, 7, 5, 4)] %>%
    # ddply(., c("type")) 
}, x = territories_area, y = defo_terr_tot, z = cells_territories, SIMPLIFY = T)


#Cleaning example (Chiribiquete) - for this you have to run first polygon_cleaning.R script

#Preapre polygons
polygons <- list(list_polygons[[8]], list_polygons_territories[[2]][[650]], list_polygons_territories[[2]][[466]]) %>%
  lapply(., function(x){
    spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  tidy()
  })

chiribiquete_clean <- list_polygons_clean_all[[8]] %>%
  spTransform(CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  tidy() %>% mutate(type = "Frontera efectiva")

polygons_centroids <-  list(list_polygons[[8]], list_polygons_territories[[2]][[650]], list_polygons_territories[[2]][[466]]) %>%
  mapply(function(x, names){
    spTransform(x, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
      coordinates() %>%
      as.data.frame() %>%
      mutate(ID = names) %>%
      rename(long = V1, lat = V2)
    }, x = ., names = c("PNN Chiribiquete", "R.I. Mirití-Paraná", "R.I. Vaupés"), SIMPLIFY = F) %>%
  ldply()


#Get GoogleMaps map
terrain_map <- get_map(location = c(-76, -0.5, -68, 2), zoom = 8, maptype = 'terrain')
plot(terrain_map)
#  left/bottom/right/top 

setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot() 
# g <- ggmap(terrain_map)
# g <- g + geom_polygon(aes(long, lat, group = id), fill = NA, colour = "orange", data = tidy(territories[[2]]))
g <- g + geom_polygon(aes(long, lat), fill = "springgreen4", data = polygons[[1]])
g <- g + geom_polygon(aes(long, lat), fill = "grey70", data = polygons[[2]])
g <- g + geom_polygon(aes(long, lat), fill = "grey70", data = polygons[[3]])
g <- g + geom_point(aes(long, lat, colour = type), data = chiribiquete_clean, size = 1, colour = "darkorange2")
g <- g + theme(axis.text = element_blank(), axis.title=element_blank(),
               panel.background = element_rect(), panel.grid.major = element_blank())
               # ,panel.grid.minor = element_blank())
g <- g + geom_text(aes(label = ID, x = long, y = lat), data = polygons_centroids)
g <- g + scalebar(x.min = -74, x.max = -69, y.min = -0.8, y.max = 2.1, 
                  dist = 50, model = "WGS84", dd2km = T, st.size = 3,
                  anchor = c(y = -2 , x = -69))
# g <- g + coord_map(xlim = c(-76, -68), ylim = c(-0.5, 2))
north2(g, x = 0.9, y = 0.9, scale = 0.1, symbol = 1.2)
ggsave(str_c("chiribiquete_clean.pdf"), width=30, height=20, units="cm")



