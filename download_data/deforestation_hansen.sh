#!/usr/bin/env bash
# file: deforestation_hansen.sh

# Script to download Hansen's deforestation data [Loss forest/year and Treecover in baseline] 

# --> This script is valid for the Global Forest Change 2000-2016 data 
#     from Hansen, et. al., (2000) in its version 1.4.

#Change directory from root and create variables
cd

if [! -d $DATA_FOLDER/hasen_raw]
then
	mkdir -p $DATA_FOLDER/hansen_raw
fi

cd $ROOT_FOLDER/download_data

colombia_grids="20N_090W|10N_090W|00N_090W|20N_080W|10N_080W|00N_080W|20N_070W|10N_070W|00N_070W"
 
#Download list of urls (each corresponding to a cell in a gridded map)
cat hansen_download_urls.txt | xargs -n 1 curl -O -k

#Select Colombia's grids from the lists and download .tifs in a folder named "Hansen_raw"

cat treecover2000.txt lossyear.txt | grep -E $colombia_grids > colombia_grids_hansen.txt
cd ~/$DATA_FOLDER/hansen_raw
cat ~/$ROOT_FOLDER/download_data/colombia_grids_hansen.txt | xargs -n 1 curl -O -k

