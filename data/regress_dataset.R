##############################################################################################
##############################################################################################
###                            CREATE DATAFRAMES FOR REGRESSION                            ###
###     THIS CODE CREATES A YEAR DATAFRAME FOR EACH TERRITORY ACCORDING TO THE NEARER      ###
###     PIXEL TO THE DISCONTINUITY BORDER CLEANED WITH THE PREVIOUS CODE                   ###
##############################################################################################
##############################################################################################

rm(list=ls())
library(data.table)
library(rgdal)
library(rgeos)
library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(foreign)
library(rlist)

# Load functions in R
setwd("~/deforestacion/")
setwd(Sys.getenv("DATA_FOLDER"))

# Path viejo
setwd("/Users/leonardobonilla/Dropbox/CEER v2/Papers/Deforestacion/Datos/")
# setwd("D:/Users/lbonilme/Dropbox/CEER v2/Papers/Deforestacion/Datos/")


###############################################################################
######### READ SHAPEFILES: NATURAL PROTECTED AREAS AND ARRANGE DATA ###########
###############################################################################

#Open natural parks shapefile (2 SP object, 1. Projected in meters and 2. Mercator)
natural_parks <- readOGR(dsn = paste0("UNEP", "/", "WDPA_June2016_COL-shapefile"), 
                         layer = "WDPA_June2016_COL-shapefile-polygons",
                         stringsAsFactors = F)
natural_parks_proj <- spTransform(natural_parks, CRS=CRS("+init=epsg:3857")) #Projection in meters

#Remove NP that are out of continental land and parks after 2016
natural_parks <- list(natural_parks, natural_parks_proj) %>%
  lapply(., function(x){
    x[!(x@data$NAME %in% c("Malpelo Fauna and Flora Sanctuary", 
                           "Old Providence Mc Bean Lagoon",
                           "Malpelo",
                           "Jhonny Cay Regional Park",
                           "The Peak Regional Park",
                           "Corales De Profundidad",
                           "Los Corales Del Rosario Y De San Bernardo",
                           "Gorgona",
                           "Acandi Playon Y Playona",
                           "Uramba Bahia Malaga")) & !x@data$STATUS_YR > 2016 & !x@data$GIS_AREA < 1 , ]
  })


###############################################################################
#################################### NOTE #####################################
# 1. FOR IDENTIFICATION PURPOSES, AN ID VARIABLE IS CREATED TO EACH POLYGON
# USING THE ROW.NUMBER.
# 2. THIS ID IS PASSED ALSO TO THE LIST ID - THAT WAY WE CAN IDENTIFY TO WHICH
# PARK THE DATA IS RELATED TO.
###############################################################################
###############################################################################

natural_parks[[2]]@data$ID <- c(1:dim(natural_parks[[2]])[1])
natural_parks[[1]]@data$ID <- c(1:dim(natural_parks[[1]])[1])

# Write metadata to dta (run panels in Stata)
write.dta(natural_parks[[1]]@data, paste0("UNEP", "/", "natural_parks.dta"))

vars <- c("ID","DESIG","STATUS_YR","GOV_TYPE")
parks_b <- natural_parks[[1]]@data[vars] %>%
  mutate(DESIG2 = mapvalues(.$DESIG, levels(as.factor(.$DESIG)), c(1:15)))
# desig <- table(parks_b$DESIG)
# summary(parks_b)
# table(parks_b$DESIG,parks_b$DESIG2)

  
# Pixel by year (stock) and type of park (and also for groups of park types)
all <- c(1:15)
national <- c(2,5,7,8,11,12,13,14,15)
regional <- c(1,3,4,6,10)
private <- c(9)

###############################################################################
########### LOOP BETWEEN YEARS AND PARK TYPES TO CREATE DATA FRAMES ###########
###############################################################################
# THIS LOOP CREATES DATAFRAMES (SAVED AS RDS R FILES) FOR EACH YEAR AND TYPE  #
# OF NATURAL PARK (REGIONAL, NATIONAL, OTHERS), THIS CODE READS THE DISTANCE  #
# FILE AND SELECTS FOR EACH YEAR AND TYPE THE NEARER EFFECTIVE FRONTIER. THIS #
# WAY ALLOW US TO CORRECT IDENTIFY THE PIXEL AND ITS FEATURES                 #
###############################################################################

############# LOAD DISTANCE DATA BASE AND MERGE WITH PARK FEATURES ############
dist <- fread("Dataframes/Estrategia 2/distance_dataframe.csv")
dist_merge <- merge(dist, parks_b, by.x = "buffer_id", by.y = "ID") %>%
  mutate(STATUS_YR = as.numeric(STATUS_YR)) %>%
  mutate(DESIG2 = as.numeric(DESIG2))

####################### CREATE DATABASES PER YEAR/DESIG ########################
# This loop takes the merged data and split it by year in a recursive way, and
# later create a nested list with two indices: year and type of area by its 
# desig code (1-15). 
################################################################################

span_years <- c(2000:2016)

dist_merge_years <- sapply(span_years, function(year){
  print(year)
  #Subset in time
  df_yr <- dist_merge %>% dplyr::filter(., STATUS_YR < year)
  #Get list of area types by year
  types <- unique(df_yr[,'DESIG2']) %>% sort(.)
  names(types) <- types
  print(types)
  #Subset in type and time
  df_yr_type_total <- types %>% sapply(function(area_type){
    df_yr_type <- df_yr %>% filter(DESIG2 == area_type) %>%
      setorder(., ID, -treatment, layer) %>%
      #Order by pixel and select the nearest contol one.
      group_by(ID) %>% filter(row_number(ID) == 1) %>%
      ungroup()
   }, simplify=F, USE.NAMES=T)
  return(df_yr_type_total)
})

mapply(function(year_name, list){
  saveRDS(list, str_c("Dataframes/Estrategia 2/dist_", year_name, ".rds"))
}, year_name = span_years, list = dist_merge_years)

########################## BIND DATA BY AREA TYPE ###########################
# This loop takes the dist_merge_years object, filter the area corresponding
# lists and bind them to get a area df per year. As with years, we select
# only relevant pixels. 
#############################################################################


areas <- list(all, national, regional, private)

dist_merge_areas_all <- mapply(function(x, year){
  all <- x[as.character(areas[[1]])] %>%
    do.call(rbind, .) %>%
    setorder(ID,-treatment, layer) %>% 
    group_by(ID) %>% 
    filter(row_number(ID) == 1)
  print(year)
  saveRDS(all, paste0("Dataframes/Estrategia 2/dist_", year, "_all.rds"))
}, x = dist_merge_years, year = span_years)

dist_merge_areas_national <- mapply(function(x, year){
  national <- x[as.character(areas[[2]])] %>%
    do.call(rbind, .) %>%
    setorder(ID,-treatment, layer) %>% 
    group_by(ID) %>% 
    filter(row_number(ID) == 1)
  print(year)
  saveRDS(national, paste0("Dataframes/Estrategia 2/dist_", year, "_national.rds"))
}, x = dist_merge_years, year = span_years)

dist_merge_areas_regional <- mapply(function(x, year){
  regional <- x[as.character(areas[[3]])] %>%
    do.call("rbind", .) %>%
    setorder(ID,-treatment, layer) %>% 
    group_by(ID) %>% 
    filter(row_number(ID) == 1)
  print(year)
  saveRDS(regional, paste0("Dataframes/Estrategia 2/dist_", year, "_regional.rds"))
}, x = dist_merge_years, year = span_years)


dist_merge_areas_private <- mapply(function(x, year){
  df_private <- x[["9"]]
  print(is.null(df_private))
  if(!is.null(df_private)){
    private <- df_private %>%
      setorder(ID,-treatment, layer) %>% 
      group_by(ID) %>% 
      filter(row_number(ID) == 1)
    print(year)
    saveRDS(private, paste0("Dataframes/Estrategia 2/dist_", year, "_private.rds"))
  } else {
    "No private areas to rbind"
  }
}, x = dist_merge_years, year = span_years)


############################## SATNITY CHECK ######################################
np_2000 <- natural_parks[[1]]@data %>%
  mutate(STATUS_YR = as.numeric(STATUS_YR)) %>%
  mutate(DESIG2 = mapvalues(.$DESIG, levels(as.factor(.$DESIG)), c(1:15))) %>%
  mutate(government = ifelse(DESIG2 %in% areas[[2]], "national",
                             ifelse(DESIG2 %in% areas[[3]], "regional", "private"))) %>%
  dplyr::filter(., STATUS_YR < 2000) %>%
  group_by(DESIG2, government) %>%
  summarize(count = n())

###############################################################################
########### LOOP BETWEEN YEARS AND PARK TYPES TO CREATE DATA FRAMES ###########
###############################################################################
# AS THE PREVIOUS LOOP, THIS LOOP DO THE SAME BUT FOR LOGITUDINAL DATA TO RUN #
# PANEL MODELS FOR EACH TYPE OF PROTECTED AREA                                #
###############################################################################

# for all, national, regional

areas <- c("all","national","regional","private")

# test

for(d in c(1:2)) { 
  print(paste0("distance ",d))
  for(a in areas) {
  print(paste0("area ",a))
  dist_panel <- list()
  for(y in 2001:2016) {
    print(paste0("year ",y))
    dist_temp <- readRDS(paste0("Dataframes/","Estrategia ",d,"/dist_",y,"_",a,".rds"))
    dist_temp$dist <- dist_temp$layer
    dist_temp <- dist_temp[dist_temp$dist<=20000,]
    dist_temp$year <- y
    dist_panel[[y-2000]] <- dist_temp
    }
  dist_panel <- do.call(rbind, dist_panel)

  # Balanced panel: 
  # Borrar ID = 0 
  # treatment = 0 if park did not exit in year 
  # desig_2016 = Time-invariant type of park (Use the 2016 park)
  # Desig_first = first year of desig if multiple 
  
  dist_panel <- dist_panel[dist_panel$ID!=0, ]
  iddat <- expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
  dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
  dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 

  dist_panel$desig_first <- dist_panel$STATUS_YR
  dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2016 
  dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))

  desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","buffer_id","dist","DESIG2")]
  names(desig_2016) <- c("ID","buffer_id_2012","dist_2012","DESIG2_2012")
  dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")

    desig_2016 <- dist_panel[dist_panel$year==2016,c("ID","buffer_id","dist","DESIG2")]
  names(desig_2016) <- c("ID","buffer_id_2016","dist_2016","DESIG2_2016")
  dist_panel <- merge(dist_panel, desig_2016, all.x=TRUE, all.y=TRUE, by="ID")

  print(dim(dist_panel))
  saveRDS(dist_panel, file =  paste0("Dataframes/","Estrategia ",d,"/dist_panel_",a,".rds"))
  }
}


# # for each area (1-15)
# 
# for(d in c(2:2)) { 
#   print(paste0("distance ",d))
#   
#   for(a in 1:15) {
#     print(paste0("area ",a))
#     dist_panel <- list()
#     for(y in 2001:2016) {
#       print(paste0("year ",y))
#       dist_temp <- readRDS(paste0("Dataframes/","Estrategia ",d,"/dist_",y,".rds"))[[a]]
#       if ((dim(dist_temp)[1]) > 0){
#         dist_temp <- dist_temp[dist_temp$dist<=20000,]
#         dist_temp$year <- y
#         dist_panel[[y-2000]] <- dist_temp
#       }
#     }
#     
#     if (length(dist_panel)>1) {
#       dist_panel <- do.call(rbind, dist_panel)
#       
#       # Balanced panel: treatment = 0 if park did not exit in year y, and Time-invariant type of park and distance (Use the 2012 park)
#       iddat <- expand.grid(ID = unique(dist_panel$ID), year = c(2001:2012))
#       dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
#       dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 
# 
#       dist_panel$desig_first <- dist_panel$STATUS_YR
#       dist_panel$desig_first[is.na(dist_panel$desig_first)] <- 2016 
#       dist_panel <- dist_panel %>% group_by(ID) %>% mutate(.,desig_first = min(desig_first))
#       
#       desig_2016 <- dist_panel[dist_panel$year==2016,c("ID","buffer_id","dist","DESIG2")]
#       names(desig_2016) <- c("ID","buffer_id_2016","dist_2016","DESIG2_2016")
#       dist_panel <- merge(dist_panel, desig_2016, all.x=TRUE, all.y=TRUE, by="ID")
#       print(dim(dist_panel))
#       saveRDS(dist_panel, file =  paste0("Dataframes/","Estrategia ",d,"/dist_panel_",a,".rds"))
#     }
#   }
# }



########################################################

# Miscellaneous 

########################################################

# pixel as any type of park in 2000

dist_2000_i <- list()

for(i in levels(dist_2000$DESIG2)){
  print(i)
  dist_temp <- dist_2000[dist_2000$DESIG2==i,]
  dist_temp$dup <- duplicated(dist_temp$ID)
  print(table(dist_temp$dup))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  dist_2000_i[[i]] <- dist_temp
}

# pixel by year (stock): all parks 


for(y in 2000:2012) {
  # for all types
  print(paste0("year ",y))
  eval(parse(text=paste("dist_temp <- do.call(rbind, dist_",y,")", sep="")))
  setorder(dist_temp, ID,-treatment,dist)
  dist_temp <- dist_temp %>% group_by(ID) %>% filter(row_number(ID) == 1)
  eval(parse(text=paste("saveRDS(dist_temp, file =  paste0(data, \"dist_",y,"_all.rds\"))", sep="")))
} 

# Reopen dist big files
for(y in 2000:2012) {
  print(paste0("year ",y))
  eval(parse(text=paste("dist_",y," <- readRDS(paste0(data,\"dist_",y,".rds\"))", sep="")))
  }  


# Regional parks in all dataset (2000)

regional <- c("Distritos De Conservacion De Suelos",
              "Distritos Regionales De Manejo Integrado",
              "Parque Natural Regional",
              "Reservas Forestales Protectoras Regionales",
              " A\u0081reas De Recreacion",
              "Reserva Forestal Protectora Nacional")
defo_dist$regional <- ifelse(defo_dist$DESIG %in% regional, 1 , 0)

# Panel for all

for(y in 2001:2012) {
  print(paste0("year ",y))
  eval(parse(text=paste("dist_temp <- readRDS(paste0(data,\"dist_",y,"_all.rds\"))", sep="")))
  dist_temp <- dist_temp[dist_temp$dist<=10000,]
  dist_temp$year <- y
  print(dim(dist_temp))
  dist_panel[[y-2000]] <- dist_temp
}

dist_panel <- do.call(rbind, dist_panel)
dim(dist_panel)
saveRDS(dist_panel, file =  paste0(data, "dist_panel_all.rds"))


# Create balanced panel
dist_panel <- readRDS(paste0(data,"dist_panel_all.rds"))
table(dist_panel$year)

iddat = expand.grid(ID = unique(dist_panel$ID), year = unique(dist_panel$year))
dist_panel <- merge(dist_panel, iddat, all.x=TRUE, all.y=TRUE, by=c("ID", "year"))
table(dist_panel$treatment, useNA = "always")

dist_panel$treatment[is.na(dist_panel$treatment)] <- 0 
table(dist_panel$treatment, useNA = "always")

desig_2012 <- dist_panel[dist_panel$year==2012,c("ID","DESIG2")]
names(desig_2012) <- c("ID","DESIG2_2012")
dist_panel <- merge(dist_panel, desig_2012, all.x=TRUE, all.y=TRUE, by="ID")



