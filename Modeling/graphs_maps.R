library(rgeos)
library(tidyr)
library(rgdal)
library(ggplot2)

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
black_territories <- readOGR(dsn = "Comunidades", layer="Tierras de Comunidades Negras (2015) ")
indigenous_territories <- readOGR(dsn = "Resguardos", layer="Resguardos Indigenas (2015) ") 
colnames(indigenous_territories@data)[8] <- "RESOLUCION"

#Aggregate area data per year for territories 

territories <- 
  list(black_territories, indigenous_territories) %>%
  lapply(spTransform, CRS=CRS("+init=epsg:3857")) %>%
  lapply(., function(x){
    mutate(x@data, year = str_replace_all(str_extract(x@data$"RESOLUCION", "[-][1-2][0, 9][0-9][0-9]"), "-", "")
           , area = gArea(x, byid = T) / 1e6) %>%
      group_by(year) %>%
      summarize(total_area = sum(area))
}) %>%
  Reduce(function(...) merge(..., by = "year", all = T, suffixes = c("_black", "_indigenous")), .)

# Aggregate area data for natural parks  

parks <- natural_parks[[2]]@data %>%
  mutate(., area = gArea(natural_parks[[2]], byid = T) / 1e6) %>%
  group_by(STATUS_YR) %>%
  summarize(total_area_parks = sum(area))

#Merge all

all <- merge(parks, territories, by.y = "year", by.x = "STATUS_YR", all = T) %>%
  gather(., type, total_area, total_area_parks:total_area_indigenous) %>%
  .[complete.cases(.), ]

setwd("~/Dropbox/BANREP/Deforestacion/Results/Graphs:Misc/")
g <- ggplot(all, aes(y = total_area, x =  as.numeric(STATUS_YR), colour = type))
g <- g + geom_line(size = 1)
g <- g + labs(x="Year", y= "Area (Km2)", title = "Total area per territory type \n(1996 - 2015)") 
g <- g + scale_colour_discrete(name = "Type of territory", breaks = c("total_area_black", "total_area_indigenous", "total_area_parks"),
                             labels = c("Afro Communitary", "Indigenous", "Parks"))
g <- g + theme_bw()
g <- g + theme(legend.position = c(.85, .85))
g
ggsave(str_c("total_areas.pdf"), width=30, height=20, units="cm")

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





