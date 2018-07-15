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


########################### RD_TO_DF_2 UTILS #########################
# This functions are auxiliary function to extract values from rd
# objects and calculate mean baselines data
#####################################################################
extract_values <- function(x, round = FALSE, digits = NULL){
  
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
  
  if(is_false(round)){
    # Create a df to start the table
    df <- data.frame(
      coef ,
      std_err,
      t_stat ,
      p_value ,
      n_treat = x$Nh[1],
      n = n_eff,
      ci_l = c_int_l,
      ci_r = c_int_r,
      bw
    )
  } else {
    if(is.null(digits)){
      digits = Inf
    }
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
      bw = round(bw, digits)
    )

  }
  
  return(df)
}


########################### RD_TO_DF_2 ###########################
# This function takes a list of rd objects and convert each element
# of the list as a model column in a named data.frame. This table 
# adds baseline control mean for the dependent variable, the second
# argument must be a list of dataframes corresponding to each of the
# models in the list_rd_objects. 
###################################################################

rd_to_df_2 <- function(list_rd_files, 
                       control_df,
                       names = NULL,
                       digits = 3,
                       baseline_variable=NULL,
                       stargazer = FALSE,
                       ...){
  
  # New rd_objects have a non straightforward way to extract estimate tables
  # from the models. Now, the process is longer.
  
  if(is_string(list_rd_files)){
    print('rd_files')
    files_data <- readRDS(list_rd_files)
    dates_files <- lapply(list_rd_files, function(x){
      file.info(x)[,'mtime'] %>% as.Date() 
    }) %>% .[[1]] 
    
    
  } else if(is.list(list_rd_files)){
    print('List')
    files_data <- list_rd_files
    dates_files <- rep(0, length(list_rd_files))
    
  } else {
    print('rd_object')
    files_data <- lapply(list_rd_files, readRDS) %>%
      unlist(., recursive=F)
  }
  
  # dates_files <- lapply(list_rd_files, function(x){
  #   file.info(x)[,'mtime'] %>% as.Date() 
  # }) %>% .[[1]] 
  
  
  if(!is.null(names)){
    if(length(names) != length(files_data)){
      stop("Names lenght do not coincide with the lenght of the lists of rd objects")
    }
  }
  
  if(is.null(names)){
    warning("No names specified")
    names <- length(files_data)
  }
  
  # Extract estimate statistics
  extract_values <- lapply(files_data, function(x){
    
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
      bw = round(bw, digits)
    )
    return(df)
  })
  
  if(!is.null(baseline_variable)){
    # Mean baseline (dependent variable)
    print("Estimating baseline deforestation per territory/park")
    defo_mean <- mapply(function(x, y){
      bw <- x$bws[1]
      y %>%
        filter(abs(dist_disc) <= bw & treatment == 0) %>% 
        summarize(mean = mean(UQ(sym(baseline_variable))))
    }, x = files_data , y = control_df, SIMPLIFY = F) %>% 
      unlist() 
    
    # Round baseline means
    defo_mean <- defo_mean %>% round(., digits)
  } else {
    defo_mean <- rep(0, length(files_data))
  }
  
  
  # Concatenate data.frames and order them
  df_concat <- extract_values %>% 
    ldply() %>% 
    cbind(., defo_mean) %>%
    see_the_stars(., beta = "coef", stat = "p_value") %>%
    parenthesis(., "std_err") %>%
    mutate(date = as.character(dates_files)) %>%
    mutate_if(., is.integer, function(x) format(x, big.mark=',')) %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(., names) %>%
    set_rownames(.,c("Treatment", "StdErr", "t-stat", "p_value", "N_treated", "N", "CI_left", "CI_right", "bws", "y_control", "model_run_date"))

  if(stargazer == TRUE){
    df_concat_sg = df_concat %>% cat(stargazer(., summary=FALSE))
    return(df_concat_sg)
  } else {
    return(df_concat)
  }
}



########################### RD_TO_DF_PLOT ###########################
# This function takes a list of rd objects and convert each element
# of the list as a model column in a named data.frame. This table 
# adds baseline control mean for the dependent variable, the second
# argument must be a list of dataframes corresponding to each of the
# models in the list_rd_objects. 
###################################################################

rd_to_plot <- function(variable, variable_name, list_df = defo_dist_all, ...){
  
  l <- lapply(list_df, function(x){
    df <- x
    df_dist <- x[, "dist_disc"]
    
    mutate(df, bin = cut(df_dist, breaks = c(-50:50), include.lowest = T)) %>%
      group_by(bin) %>%
      summarize(meanbin = mean(UQ(sym(variable))), sdbin = sd(UQ(sym(variable))), n = length(ID)) %>%
      .[complete.cases(.),] %>%
      as.data.frame() %>%
      mutate(treatment = ifelse(as.numeric(row.names(.)) > 50, 1, 0), bins = row.names(.)) %>%
      mutate(bins = mapvalues(.$bins, from = c(1:100), to = c(-50:49)))
  })
  
  
  library(tidyr)
  l_all <- data.frame(l) %>%
    select(-bin.1, -bin.2, -bin.3, -bins.1, -bins.2, -bins.3, -treatment.1, -treatment.2, -treatment.3) %>%
    gather(key, value, -bins, -bin, -treatment) %>%
    separate(key, c("variable", "type")) %>%
    mutate(type = ifelse(is.na(type), 0, type)) %>%
    spread(variable, value) %>%
    mutate(type = as.factor(type)) %>% mutate(type = mapvalues(type, from = c(0, 1, 2, 3), 
                                                               to = c("National Protected Areas",
                                                                      "Regional Protected Areas",
                                                                      "Indigenous reserves", 
                                                                      "Black communities reserves")))
  
  
  
  
  g <- ggplot(l_all, aes(y = meanbin, x = as.numeric(bins), colour = as.factor(treatment))) 
  g <- g + facet_wrap( ~ type, ncol=2)
  g <- g + stat_smooth(method = "loess") 
  g <- g + geom_point(colour = "black", size = 1)
  #g <- g + scale_x_continuous(limits = c(-20, 20))
  #g <- g + scale_y_continuous(limits = c(0, 0.3))
  g <- g + labs(x = "Distance (Km.)", y = variable_name)
  g <- g + guides(colour = FALSE)
  g <- g + theme_bw()
  g
  ggsave(paste0("RD/Graphs/RDggplot_all_strategy2", "_", variable, ".pdf"), width=30, height=20, units="cm")
}





