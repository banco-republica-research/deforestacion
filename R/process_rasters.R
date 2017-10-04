###############################################################################
# Functions to process raster data: crop, raster and set a comparable layer to 
# combine data sources
###############################################################################

# Set extent and crop a list of raster layers to stack them
processing_rasters <- function(layer.list, ext, shape){
  layer.list %>%
    lapply(setExtent, ext) %>%
    lapply(crop, shape) %>%
    stack() %>% 
    mask(shape)
}






