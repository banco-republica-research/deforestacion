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






