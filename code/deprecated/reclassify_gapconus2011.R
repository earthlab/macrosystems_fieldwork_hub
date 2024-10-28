### ### ### ### ### ### ### ### ### ### ### ###

#Reclassify GAP CONUS 2011 raster

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#4/5/2023


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(terra)
library(sf)
library(here)
library(tidyverse)


## Clean workspace & Set env variables ----
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)


## Load data ----
gapConusSouthernRockies <- terra::rast(here('data', 'GapConusCover2011_SouthernRockies_EPSG_32613_GEE.tif'))

gapConusLookup <- read.csv(here('data', 'gap_conus_lookup_codes.csv'))

# Re-classify and output ----

#Create set of new codes
Category <- unique(gapConusLookup$Category)
newClasses <- seq(1:length(Category)) %>% as.integer()
newCodes <- cbind(newClasses, Category) %>% as.data.frame()
newCodes$newClasses <- as.integer(newCodes$newClasses)

#Bind new codes to full lookup table & create classify matrix
lookupNew <- left_join(gapConusLookup, newCodes)
lookupNew$GAP_CONUS_2011_Code <- as.integer(lookupNew$GAP_CONUS_2011_Code)
classifyM <- lookupNew %>%
  select(GAP_CONUS_2011_Code, newClasses) %>%
  as.matrix()

#Perform classify w/ terra
newGapC2011 <- gapConusSouthernRockies %>% terra::classify(classifyM)

#Write
terra::writeRaster(newGapC2011,
                   here('data', 'derived', 'classified_gap_conus_2011.tif'),
                   overwrite = TRUE,
                   datatype = "INT1U")



