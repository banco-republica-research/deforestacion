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


###############################################################################
## 		            	PERMUTATION TESTS: CANAY & CAMAT (2017) 	               ##
## This function calculates the Canay and Camat test to check violations to  ##
## the RD assumptions. The function is a simple wrapper to the RATest pkg    ##
## and will simply allow to apply the function to a list of dataframes       ##
## returning a concatenated dataframe with the statistics plus a id colum    ##
## list_df = data.frame list                    			                       ##
## names = names to add as id for each dataframe                             ##
## covs = vector of named covariates                                         ##
## c = cutoff (set as 0)                                                     ##
# '...' = other arguments passed to the RATest::RDperm function              ##
###############################################################################

perm_test_list <- function(list_df, names, covs, z, c, n){
      
      if(length(names) != length(list_df)){
        stop("Length of dataframes list do not coincide with the names one :/")
      }
      	
      else{
        perm_test <- lapply(list_df, function(x){
          p_test_element <- RATest::RDperm(W = covs,
                                           z = z,
                                           cutoff = c,
                                           n.perm = n,
                                           data = x)
          return(p_test_element['results'])
        })
        
        # Prepare data as data.frame and add covariate names into structure
        perm_df <- lapply(perm_test, function(x){
          x %>% data.frame() %>% mutate(var = row.names(.))
        })
        # Concatenate data.frames and spit out a dataframe with df ids
	names_rep <- rep(names, length(covs)+1) %>% 
		factor(., levels=names) %>% 
		sort(.) %>% as.character(.)

        perm_df_id <- perm_df %>% ldply %>% mutate(area = names_rep) %>%
          mutate(., var = str_replace_all(var, '_', ''))
        return(perm_df_id)
      }
}


descriptive_stats_buffer <- function(list_df, 
                                     dist_var, 
                                     buffer = 5, 
                                     covs, 
                                     treatment = FALSE,
                                     names,
                                     digits = Inf){
  if(is.null(covs)){
    stop('Put some variables to calculate stuff')
  }
  if(treatment==FALSE){
    warning('No specified buffer. A 5 km buffer will be used as default')
    buffer = 5
    
    filter_list <- lapply(list_df, function(x){
      print(buffer)
      
      df_sum_stats <- x %>%  
        filter(abs(UQ(sym(dist_var))) <= buffer) %>%
        dplyr::select(covs) %>%
        mutate_all(., funs(as.numeric)) %>%
        set_colnames(str_replace_all(names(.), '_', '')) %>%
        summarize_all(., funs(
                            min = min(., na.rm=T),
                            max = max(., na.rm=T),
                            mean = mean(., na.rm=T),
                            sd = sd(., na.rm=T),
                            n = n())
                       )
      
       df_shape <- df_sum_stats %>% 
         gather(stat, val) %>%
         separate(stat, into = c('var', 'stat'), sep = '_') %>%
         spread(stat, val) %>%
         dplyr::select(var, mean, sd, min, max, n) %>%
         mutate_if(is.numeric, round, digits)
         
         return(df_shape)
    })
  }
  
   named_list <- mapply(function(df, name){
     df_named <- df %>%
       mutate(area = name)
   }, df = filter_list, name = names, SIMPLIFY = F)
   
   summary_df <- named_list %>% ldply() %>%
     select(area, everything())
  
     return(summary_df)
}



###############################################################################
## 		            PLACEBO TESTS - REPEAT RD CHANGING CUT-OFF 	               ##
##  This function will calculate the treatment effect for a range of         ##
##  cut-offs at both sides of the real cut-off (here c = 0) in otder         ##
##  to validate our results (robustness)                                     ##  
###############################################################################

rd_placebos <- function(df, 
                        var_dep, 
                        start, 
                        end, 
                        step=0.5, ...){
  
  if(abs(start) != end){
    stop('Start and end must be simetrical: start < 0 < end and 
         start * -1 = end')
  }
  
  print(var_dep)
  
  results <- list()
  
  for(i in seq(start, end, step)){
    
    index = as.character((i + 1) + end)
    print(index)
    results[[index]] <- rdrobust::rdrobust(
      y = df[, var_dep],
      x = df$dist_disc,
      covs = cbind(df$altura_tile_30arc, df$slope, df$roughness, df$prec, 
                   df$sq_1km.1, df$treecover_agg),
      all = T,
      h = 5,
      c = i
    )
  }
  return(results)
  }



###############################################################################
## 		          SENSIBILITY TESTS - REPEAT RD CHANGING CUT-OFF 	             ##
##  This function will calculate the treatment effect for a range of         ##
##  cut-offs at both sides of the real cut-off (here c = 0) in otder         ##
##  to validate our results (robustness)                                     ##  
###############################################################################

rd_sensibility <- function(df, 
                           bws,
                           var_dep,
                           start, 
                           end, 
                           step=0.5, ...){

  print(var_dep)
  
  # Create bw sensibility lists
  bws_list <- lapply(bws, function(x){
    c(seq(start, end, by = step), x) %>%
      .[sort.list(.)]
  })
  
  results <- lapply(bws_list, function(x){
    lapply(x, function(bw_vector){
      print(bw_vector)
      print(var_dep)
      rdrobust(
        y = df[, var_dep],
        x = df$dist_disc,
        covs = cbind(df$altura_tile_30arc, df$slope, df$roughness, df$prec, 
                     df$sq_1km.1, df$treecover_agg),
        all = T,
        h = bw_vector,
        vce = 'hc1',
        c = 0)
    })
  })
  
  return(results)
}
