###############################################################################
###############################################################################
##        REGRESSION DISCONTINUITY PLOTS USING THE RDROBUST PACKAGE          ##
## This code will generate plots calling a ploting function from the         ##
## rdrobust package. We will plot an rd for each of the areas for our main   ##
## models                                                                    ##
###############################################################################
###############################################################################

library(plyr)
library(dplyr)
library(rdrobust)
library(stargazer)
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)
library(rlang)
library(purrr)



# Source tables functions
setwd(Sys.getenv("ROOT_FOLDER"))
source("R/func_tables.R")
source("modeling/merge_datasets.R")

# Set directories
setwd(paste0(Sys.getenv("OUTPUT_FOLDER")))

x<-runif(1000,-1,1)
y<-5+2*x+3*(x>=0)+rnorm(1000)
rdplot(y,x)

par(mfrow = c(2,1))
rdplot(x = defo_dist[[1]]$dist_disc,
       y = defo_dist[[1]]$loss_sum,
       p = 4,
       ci = 95,
       binselect = "es",
       x.lim = c(-50, 50),
       # title = str_c("Regression discontinuity for", type, sep = " "),
       x.label = "Distance to territory border (Km)",
       y.label = expression(paste('Deforestation Rate (', ha/km^2, ')')),
       c = 0)
rdplot(x = defo_dist[[2]]$dist_disc,
       y = defo_dist[[2]]$loss_sum,
       p = 4,
       ci = 95,
       binselect = "es",
       x.lim = c(-50, 50),
       # title = str_c("Regression discontinuity for", type, sep = " "),
       x.label = "Distance to territory border (Km)",
       y.label = expression(paste('Deforestation Rate (', ha/km^2, ')')),
       c = 0)

