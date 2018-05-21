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
## 			PERMUTATION TESTS: CANAY & CAMAT (2017) 	     ##
## This function calculates the Canay and Camat test to check violations to  ##
## the RD assumptions. The function is a simple wrapper to the RATest pkg    ##
## and will simply allow to apply the function to a list of dataframes       ##
## returning a concatenated dataframe with the statistics plus a id colum    ##
## list_df = data.frame list                    			     ##
## names = names to add as id for each dataframe                             ##
## covs = vector of named covariates                                         ##
## c = cutoff (set as 0)                                                     ##
# '...' = other arguments passed to the RATest::RDperm function              ##
###############################################################################

perm_test_list <- function(list_df, names, covs, z, c = 0, ...){
      
      if(length(names) != length(list_df)){
        stop("Length of dataframes list do not coincide with the names one :/")
      }
      	
      else{
        perm_test <- lapply(list_df, function(x){
          p_test_element <- RATest::RDperm(W = covs,
                                           z = z,
                                           cutoff = c,
                                           data = x)
          return(p_test_element['results'])
        })
        
        # Prepare data as data.frame and add covariate names into structure
        perm_df <- lapply(perm_test, function(x){
          x %>% data.frame() %>% mutate(covariate = row.names(.))
        })
        # Concatenate data.frames and spit out a dataframe with df ids
	names_rep <- rep(names, length(covs)+1) %>% 
		factor(., levels=names) %>% 
		sort(x) %>% as.character()

        perm_df_id <- perm_df %>% ldply %>% mutate(df_id = names_rep)
        return(perm_df_id)
      }
    }

