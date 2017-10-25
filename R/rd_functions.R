###############################################################################
################## FUNCTION TO MODEL REGRESSION DISCONTINUITIES ###############
###############################################################################

rd_robust <- function(df, x, y, nn, bw = NULL, covs_matrix = NULL){
  dependent <- df$y
  running <- df$x
  rdrobust::rdrobust(
    x = dependent,
    y = running, 
    covs = cbind(df[covs_matrix]),
    h = bw,
    vce = "nn",
    nnmatch = nn
  )
}
  
