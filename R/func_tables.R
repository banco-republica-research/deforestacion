################################# RD TO DATAFRAMES ############################
# This set of functions will take the RD (saved from reg_dis.R as .rds) objects 
# and convert them to readable dataframes. This also will allow us to convert 
# these results easily to LaTeX. 
##############################################################################

########################### See the stars! ###########################
# This function grabs one dataframe and adds statistical significance
# stars to the coefs. This function works inside the main function
# rd_to_df_2 below. It also can add LaTeX star code
######################################################################

see_the_stars <- function(df,
                          beta,
                          stat,
                          latex = FALSE){
  
  #NSE for dplyr (works for this version)
  if(!latex){
    df_ <- df %>%
      mutate(., coef = ifelse(
        # Not significant at 10%
        UQ(sym(stat)) > 0.1,
        paste0(UQ(sym(beta))),
        # Significant: 10%
        ifelse(
          UQ(sym(stat)) < 0.1
          & UQ(sym(stat)) > 0.05,
          paste0(as.character(UQ(sym(beta))), "*"),
          ifelse(
            # Significant 5%
            UQ(sym(stat)) < 0.05
            & UQ(sym(stat)) > 0.01,
            paste0(as.character(UQ(sym(beta))), "**"),
            ifelse(
              # Significant 1%
              UQ(sym(stat)) < 0.01,
              paste0(as.character(UQ(sym(beta))), "***"), UQ(sym(beta)))

            )
              )
                )
                  )
    return(df_)
  
   } else {
    df_ <- df %>%
      mutate(., coef = ifelse(
        # Not significant at 10%
        UQ(sym(stat)) > 0.1,
        paste0(UQ(sym(beta))),
        # Significant: 10%
        ifelse(
          UQ(sym(stat)) < 0.1
          & UQ(sym(stat)) > 0.05,
          paste0(as.character(UQ(sym(beta))), "^{*}"),
        ifelse(
          # Significant 5%
          UQ(sym(stat)) < 0.05
          & UQ(sym(stat)) > 0.01,
          paste0(as.character(UQ(sym(beta))), "^{**}"),
          ifelse(
            # Significant 1%
            UQ(sym(stat)) < 0.01,
            paste0(as.character(UQ(sym(beta))), "^{***}"), UQ(sym(beta)))
        )
          )
            )
              )

    return(df_)
   }
}


########################### Parenthesis! ###########################
# This function grabs one dataframe and adds parenthesis to its 
# standard errors. As with "see the stars", this function is called
# inside rd_to_df_2 func.
######################################################################



parenthesis <- function(df, std_err_name){
  df_ <- df %>%
    mutate(., std_err = as.character(UQ(sym(std_err_name))) ) %>%
    mutate(., std_err = paste0("(", UQ(sym(std_err_name)), ")") )
  return(df_)
}


########################### RD_TO_DF_2 ###########################
# This function takes a list of rd objects and convert each element
# of the list as a model column in a named data.frame. This table 
# adds baseline control mean for the dependent variable, the second
# argument must be a list of dataframes corresponding to each of the
# models in the list_rd_objects. 
###################################################################



rd_to_df_2 <- function(list_rd_objects, 
                       control_df,
                       names = NULL,
                       digits = 3){
  
  if(!is.null(names)){
    if(length(names) != length(list_rd_objects)){
      stop("Names lenght do not coincide with the lenght of the lists of rd objects")
      }
  }
  
  if(is.null(names)){
    warning("No names specified")
  }
  # New rd_objects have a non straightforward way to extract estimate tables
  # from the models. Now, the process is longer.
  
  # Extract estimate statistics
  extract_values <- lapply(list_rd_objects, function(x){
    # Table esential stats
    coef = x$Estimate[1, 'tau.bc']
    std_err = x$Estimate[1, 'se.rb']
    t_stat = x$Estimate[, 'tau.bc']/x$Estimate[, 'se.rb']
    p_value = 2 * pnorm(-abs(t_stat))
    bw = x$bws[1]
    n_eff = x$Nh[1] + x$Nh[2]
    
    # Additional stats
    z_stat = -qnorm(abs((1 - (x$level/100))/2))
    c_int_l = (x$Estimate[, "tau.bc"] - x$Estimate[, "se.rb"]) * z_stat
    c_int_r = (x$Estimate[, "tau.bc"] + x$Estimate[, "se.rb"]) * z_stat
   
    # Create a df to start the table
    df <- data.frame(
      coef = round(coef, digits),
      std_err = round(std_err, digits),
      t_stat = round(t_stat, digits),
      p_value = round(p_value, digits+1),
      n_treat = x$Nh[1],
      n = n_eff,
      ci_l = round(c_int_l, digits),
      ci_r = round(c_int_r, digits),
      bw = bw
    )
    return(df)
  })
  
  # Mean baseline (dependent variable)
  print("Estimating baseline deforestation per territory/park")
  defo_mean <- mapply(function(x, y){
    bw <- x$bws[1]
    y %>%
      filter(abs(dist_disc) <= bw & treatment == 0) %>% 
      summarize(mean = mean(loss_sum))
  }, x = list_rd_objects , y = control_df, SIMPLIFY = F) %>% unlist()
  
  # Concatenate data.frames and order them
  df_concat <- extract_values %>% 
    ldply() %>% 
    cbind(., defo_mean) %>%
    see_the_stars(., beta = "coef", stat = "p_value") %>%
    parenthesis(., "std_err") %>%
    t() %>%
    as.data.frame() %>%
    # mutate_all(funs(as.character)) %>%
    # mutate_all(funs(as.numeric)) %>%
    # mutate_all(funs(round), digits) %>%
    set_colnames(., names) %>%
    set_rownames(.,c("Treatment", "StdErr", "t-stat", "p_value", "N_treated", "N", "CI_left", "CI_right", "bws", "y_control"))
  
  return(df_concat)
  
}








