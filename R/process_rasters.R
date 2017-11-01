###############################################################################
# Functions to process raster data: crop, raster and set a comparable layer to 
# combine data sources
###############################################################################

# Set extent and crop a list of raster layers to stack them
processing_rasters <- function(layer.list, ext, shape){
  layer.list %>%
    lapply(raster::setExtent, ext) %>%
    lapply(raster::crop, shape) %>%
    raster::stack() %>%
    raster::mask(shape)
}

###############################################################################
# Function to process shape geometries and remove holes from SpatialPolygons 
# The function return a new SpatialPolygons object without holes. 
# [Thanks to Roger Bivand!]
###############################################################################

remove_holes <- function(x){
  BCp <- slot(x, "polygons")
  holes <- lapply(BCp, function(i) sapply(slot(i, "Polygons"), slot, "hole"))
  res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]])
  IDs <- row.names(x)
  
  if(class(x) == "SpatialPolygons" & class(x) == "SpatialPolygonsDataFrame"){
    x <- SpatialPolygons(lapply(1:length(res), function(i){
      Polygons(res[[i]], ID=IDs[i])
    }), proj4string = CRS(proj4string(x))
    )
  } else if(class(x) != "SpatialPolygonsDataFrame"){
    print("Can't remove holes. Not a polygon sp")
   
  }
  return(x)
}

###############################################################################
# Function to take polygon geom and yield a points geom. This is not a complex
# function, but saves space and readibility to the user
###############################################################################

to_points <- function(x){
  if( class(x) == "SpatialPolygons")
  x %>%
    as("SpatialLines") %>%
    as("SpatialPoints")
  else if( class(x) == "SpatialPolygonsDataFrame")
    x %>%
    as("SpatialLines") %>%
    as("SpatialPoints")
  else{
    stop("This is not a valid geom object to convert to points")
  }
}


###############################################################################
# Function to process shape geometries and take each feature to a list element 
###############################################################################

polygon_to_list <- function(shape){
  list_polygons <- list()
  for(i in shape@data$ID){
    list_polygons[[i]] <- shape[shape@data$ID == i, ]
  }
  return(list_polygons)
}

