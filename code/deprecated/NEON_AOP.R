### ### ### ### ### ### ### ### ### ### ### ###

#Download NEON AOP imagery over a set of coordinates

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#4/27/2023


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "neonUtilities")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(terra)
library(sf)
library(here)
library(tidyverse)
library(neonUtilities) #neon data utilities package, tutorial here: https://www.neonscience.org/resources/learning-hub/tutorials/neondatastackr


## Clean workspace & Set env variables ----
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)


## Set directories ----
#Set output directory & create if doesn't already exist
outDir <- here("data", "neon_aop")

# check if sub directory exists 
if (!dir.exists(outDir)){
  dir.create(here("data", "derived"))
}





# ## Use byTileAOP() to query for data tiles ---- #####NOT WORKING FOR SOME REASON?... 0 tiles found
dpID = "DP3.30010.001" #NIWO RGB mosaic unique identifier
buffer = 50
year = "2020"
site = "NIWO"
#easting, northing in UTMs - easy to pull from AOP data viewer: https://data.neonscience.org/data-products/explore
easting = 4539000
northing = 44313000

byTileAOP(dpID=dpID, site=site,
          year=year, easting=northing,
          northing=northing,
          buffer=buffer,
          savepath = outDir)




