#Calculate distances from pixels to points in parallel for each territory. 

###############################################################################
############################### NOTE ##########################################
# THIS FUNCTIONS ARE INDIVIDUALLY TAILORED FOR EACH PROTECTED AREA. THIS IS FAR
# FROM IDEAL, BUT THEY MAKE THEIR JOB. BE AWARE THAT IN ORDER TO CALCULATE 
# PROPERLT DISTANCES IN TERRITORIES, YOU SHOULD CHANGE THE RASTER LIST INDEX 
# DEPENDING ON THE TYPE: 1: BLACK, 2: INDIGENOUS
###############################################################################

  

calculate_distances_parallel <- function(buffer, points){
  if(length(points)  > 2 & typeof(points) == "S4"){
    crop(res_mask_natural_parks_buffers, buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_natural_parks_buffers)
  } else
    return(0)
}

########################## WARNING: READ NOTE #################################
################ THIS FUNCTIONS NEEDS SERIOUS EDITING TO AVOID ERRORS #########
###############################################################################


calculate_distances_parallel_territories <- function(buffer, points){
  if(length(points)  > 2 & typeof(points) == "S4"){
    crop(res_mask_buffers[[2]], buffer) %>%
      mask(buffer) %>%
      clusterR(.,distanceFromPoints, args = list(xy = points)) %>%
      mask(buffer) %>%
      resample(res_mask_buffers[[1]])
  } else
    return(0)
}