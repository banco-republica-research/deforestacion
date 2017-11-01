###############################################################################
# Functions to clean geometries: delete contiguous points and get comparable  
# boundaries. Here you can have two approaches:
# 1. Only remove Colombian borders and get bad treatments 
# 2. Remove all contiguous points and get only non-treated units (points). 
 ###############################################################################


#Clean SpatialPoints (from polygons of Natural parks) -remove points out of national frontiers and border points-
clean_treatments_border <- function(x, points_border, dist){
  # x: sp polygon object
  # points_border: a sp_polygon or sp_polygon_dataframe object to define the boundaries
  # dist: min distance between x from points_border to remove points from x
  print(x$ID)
  sp <- x %>% as("SpatialLines") %>% as("SpatialPoints")
  knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
    data.frame(.)
  sp_final <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
    .[!.@data$nn.dist < dist, ] 
  
}


#Clean SpatialPoints (from polygons of Natural parks) -remove other treatments and get effective boundaries-
clean_treatments <- function(x, polygon, points_sp, points_border, shape){
  print(x$ID)
  if(gIntersects(x, polygon)){
    #Remove inside points
    dif <- gDifference(x, polygon, drop_lower_td = T)
    if(!is.null(dif)){
      dif <- tidy(dif)[, 1:2] #Coordinates difference
      polygon2_coords <- tidy(x)[,1:2] #Coordinates polygon
      # Duplicated_coords is the non-intersecting points of the polygon2
      duplicated_coords <- merge(dif, polygon2_coords) 
      if(dim(duplicated_coords)[1] > 0){
        res <- SpatialPoints(duplicated_coords, proj4string = CRS("+init=epsg:3857"))
      } else {
        res <- SpatialPoints(polygon2_coords, proj4string = CRS("+init=epsg:3857"))
      }
      
    } else {
      return(0)
    }
    #Remove close cofounding treatments
    knn <- get.knnx(coordinates(points_sp), coordinates(res), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp <- SpatialPointsDataFrame(res, knn, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_border <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    dif <- gDifference(shape, x) %>% as("SpatialLines") %>% as("SpatialPoints")
    knn_final <- get.knnx(coordinates(dif), coordinates(sp_border), k = 1, algorithm = "kd_tree") %>%
      data.frame()
    sp_final <- SpatialPointsDataFrame(sp_border, knn_final, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 500, ]
    
  } else {
    # Remove close cofounding treatments
    points <- x %>% as("SpatialLines") %>% as("SpatialPoints")
    knn <- get.knnx(coordinates(points_sp), coordinates(points), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp <- SpatialPointsDataFrame(points, knn, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    knn_border <- get.knnx(coordinates(points_border), coordinates(sp), k = 1, algorithm = "kd_tree") %>%
      data.frame(.)
    sp_border <- SpatialPointsDataFrame(sp, knn_border, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 1000, ]
    dif <- gDifference(shape, x) %>% as("SpatialLines") %>% as("SpatialPoints")
    knn_final <- get.knnx(coordinates(dif), coordinates(sp_border), k = 1, algorithm = "kd_tree") %>%
      data.frame()
    sp_final <- SpatialPointsDataFrame(sp_border, knn_final, proj4string = CRS("+init=epsg:3857")) %>%
      .[!.@data$nn.dist < 500, ]
  }
  
}
