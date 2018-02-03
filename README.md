# ¿Does national protect areas work for deter deforestation? :evergreen_tree: :evergreen_tree: :evergreen_tree:

This repository contains all the replication code for the paper, *Protected Areas 
under Weak Institutions: evidence from Colombia*. This project assess the effect 
of different designs of protection over deforestation in Colombia. 
we use two different types of evaluation. First, we use regression discontinuities
to assess local effects of protecting land in the long term. Second, we exploit the 
the timing in area enactment to compare short term effects. Results shows that natural 
protected areas, such as Natural and Regional Parks have a positive effect in 
curtailing deforestation in the long term. Nonetheless, no effects are founded for 
Black reserves.

A novel result of this paper is that, contrary to the common logic, human settlements
and roads have a positive effect. This is explained by the nature of the deforestation
in Colombia, which is mainly driven by coca crops and illegal mining. Hence, cities and 
high density road zones, in which institutional frameworks are strong, helps to a 
better enforcement of the protection. 


## How to run this code?
Running this code requires R (3.4.1) and Python 3.5. Feature extracting, raster processing, 
and modeling were performed in R. Rasterization of UNODC data on coca crops and illegal
mining was performed in Python. Some additional analysis in the paper were calculated using 
Stata (TODO: do this on R). Also, since we can't upload the complete raw replication databases
we use Dropbox to store our data.    

## Environment variables [only valid for Unix systems]
To run this repository in a efficient way and in multiple platforms. We create an `environment_
variables` script which creates a `.Renviron` local file. R can read this file and we can call
environment variables from R code. To use this file you can `source` it and specify the path
to the data folder in your Dropbox folder. The script will create personalized environment 
variables for each user and make replication easier. 



