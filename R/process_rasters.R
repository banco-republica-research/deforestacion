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
###############################################################################

remove_holes <- function(x){
  BCp <- slot(x, "polygons")
  holes <- lapply(BCp, function(i) sapply(slot(i, "Polygons"), slot, "hole"))
  res <- lapply(1:length(BCp), function(i) slot(BCp[[i]], "Polygons")[!holes[[i]]])
  IDs <- row.names(x)
  
  if(class(x) == "SpatialPolygons"){
    x <- SpatialPolygons(lapply(1:length(res), function(i){
      Polygons(res[[i]], ID=IDs[i])
    }), proj4string = CRS(proj4string(x))
    )
  } else if(class(x) == "SpatialPolygonsDataFrame"){
    x <- SpatialPolygonsDataFrame(lapply(1:length(res), function(i){
      Polygons(res[[i]], ID=IDs[i])
    }), data = slot(x, "data")
    )
  }
  return(x)
}



