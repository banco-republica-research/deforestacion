library(rgeos)
library(dplyr)
library(forcats)
library(tidyr)
library(rgdal)
library(forcats)
library(xtable)
library(ggplot2)
library(ggsn)

# Maps and Facts

########################
# Total area in hectares per year (for parks and territories)


# Open shapefiles
setwd("~/Dropbox/BANREP/Deforestacion/Datos/UNEP")
natural_parks <- readOGR(dsn = "WDPA_June2016_COL-shapefile", layer = "WDPA_June2016_COL-shapefile-polygons")
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Remove NP that are out of continental land (Malpelo and Providencia)
natural_parks <- list(natural_parks, natural_parks_proj) %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park")), ]
  })

setwd("/Users/Ivan/Dropbox/BANREP/Deforestacion/Datos")
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015)")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015)") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"

#Aggregate area data per year for territories 

territories <- 
  list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+init=epsg:3857")) %>%
  lapply(., function(x){
    mutate(x@data, STATUS_YR = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", "")
           , area = gArea(x, byid = T) / 1e6) %>%
      group_by(STATUS_YR) %>%
      summarize(total_area = sum(area))
}) %>%
  Reduce(function(...) merge(..., by = "STATUS_YR", all = T, suffixes = c("_black", "_indigenous")), .) %>%
  gather(., type, total_area, total_area_black, total_area_indigenous) %>%
  mutate(., type = as.factor(type)) %>% mutate(., type = fct_recode(type, "Comunidades Negras" = "total_area_black", 
                                                                   "Resguardos Indígenas" = "total_area_indigenous"),
                                               STATUS_YR = as.integer(STATUS_YR))
  

# Aggregate area data for natural parks  
regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion")


parks <- natural_parks[[2]]@data %>%
  mutate(., area = gArea(natural_parks[[2]], byid = T) / 1e6) %>%
  mutate(., regional = ifelse(DESIG %in% regional, 1, 0)) %>%
  mutate(regional = as.factor(regional)) %>% mutate(.,type = fct_recode(regional, Regional = "1", Nacional = "0")) %>%
  group_by(STATUS_YR, type) %>%
  summarize(total_area = sum(area))

#Merge all

all <- rbind(as.data.frame(parks), territories) %>%
  mutate(total_area = ifelse(is.na(total_area), 0, total_area)) %>%
  mutate(cumsum = ave(total_area, type, FUN = cumsum)) %>%
  mutate(cumsum = round(cumsum, 4)) %>%
  arrange(STATUS_YR)


setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot(all, aes(y = cumsum / 1000, x = STATUS_YR, colour = type))
g <- g + geom_line(size = 1)
g <- g + labs(x="Año", y = expression(Area~(miles~de~km^{2})))
g <- g + scale_colour_discrete(name = "Área Protegida") 
g <- g + scale_x_continuous(limits = c(1940, 2014), breaks = seq(1940, 2014, by = 5))
g <- g + geom_vline(xintercept = 2000, colour="gray", linetype = "longdash")
g <- g + theme_bw()
g <- g + theme(legend.position = c(.15, .85))
g <- g + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))
g
ggsave(str_c("total_areas_geomarea.pdf"), width=30, height=20, units="cm")

#Total deforestation year (total)

defo_year_total <- defo %>%
  select(loss_year_brick_1km.2:loss_year_brick_1km.13) %>%
  colSums(.) %>%
  data.frame( defo_year = ., year = (2001:2012)) %>%
  mutate(defo_year_ha = .$"defo_year" * 100)  %>%
  select(., year, defo_year_ha) %>%
  setNames(., c("year", "defo_year"))


# Filter defo by group

ids <- lapply(defo_dist_all, function(x){
  filter(x, treatment == 1) %>%
    .[, 1]
})


defo_year_group <- lapply(ids, function(x){
  defo[defo$ID %in% x, ] %>%
    select(loss_year_brick_1km.2:loss_year_brick_1km.13) %>%
    colSums(.) %>%
    data.frame(defo_year = ., year = (2001:2012)) %>%
    mutate(defo_year_ha = .$"defo_year" * 100) %>%
    select(., year, defo_year_ha) %>%
    setNames(., c("year", "defo_year"))
}) 
   

defo_year_group <- append(list(defo_year_total), defo_year_group) 

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



