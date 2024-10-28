# This deprecated script mosaics AOP tiles. It is now preferred to load multiple separate styles and export to tile package. Skip this step.


#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "mapview")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(terra)
library(sf)
library(here)
library(tidyverse)
library(mapview)


## Clean workspace & Set env variables ----
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)


#Load tif files selected manually for relevance (i.e. accessibility, overlap w/ USFS land)

#Lower elevation south
sLowerRast <- list.files(here('data', 'neon_aop', 'niwot_selected', 'S_LOWER'),
                      pattern = ".tif$",
                      recursive = TRUE,
                      full.names = TRUE)
sLowerSPRC <- sprc(sLowerRast)
sLowerMosaic <- mosaic(sLowerSPRC)
terra::writeRaster(sLowerMosaic, here('data', 'neon_aop', 'niwot_selected', 'S_LOWER', 'south_lower_niwot_mosaic.tif'))
rm(sLowerMosaic, sLowerSPRC)

#Lower elevation north
nLowerRast <- list.files(here('data', 'neon_aop', 'niwot_selected', 'N_LOWER'),
                         pattern = ".tif$",
                         recursive = TRUE,
                         full.names = TRUE)
nLowerSPRC <- sprc(nLowerRast)
nLowerMosaic <- mosaic(nLowerSPRC)
terra::writeRaster(nLowerMosaic, here('data', 'neon_aop', 'niwot_selected', 'N_LOWER', 'north_lower_niwot_mosaic.tif'))
rm(nLowerMosaic, nLowerSPRC)

#Rainbow lakes area
rainbowRast <- list.files(here('data', 'neon_aop', 'niwot_selected', 'RAINBOW_LAKES'),
                         pattern = ".tif$",
                         recursive = TRUE,
                         full.names = TRUE)
rainbowSPRC <- sprc(rainbowRast)
rainbowMosaic <- mosaic(rainbowSPRC)
terra::writeRaster(rainbowMosaic, here('data', 'neon_aop', 'niwot_selected', 'RAINBOW_LAKES', 'rainbow_niwot_mosaic.tif'))
rm(rainbowMosaic, rainbowSPRC)

#MRS area
mrsRast <- list.files(here('data', 'neon_aop', 'niwot_selected', 'MRS'),
                          pattern = ".tif$",
                          recursive = TRUE,
                          full.names = TRUE)
mrsSPRC <- sprc(mrsRast)
mrsMosaic <- mosaic(mrsSPRC)
terra::writeRaster(mrsMosaic, here('data', 'neon_aop', 'niwot_selected', 'MRS', 'mrs_niwot_mosaic.tif'))
rm(mrsMosaic, mrsSPRC)

#Brainard lake area
brainardRast <- list.files(here('data', 'neon_aop', 'niwot_selected', 'BRAINARD'),
                          pattern = ".tif$",
                          recursive = TRUE,
                          full.names = TRUE)
brainardSPRC <- sprc(brainardRast)
brainardMosaic <- mosaic(brainardSPRC)
terra::writeRaster(brainardMosaic, here('data', 'neon_aop', 'niwot_selected', 'BRAINARD', 'brainard_niwot_mosaic.tif'))
rm(brainardMosaic, brainardSPRC)

#Lower elevation south HP
sLowerRastHP <- list.files(here('data', 'neon_aop', 'niwot_selected', 'S_LOWER', "HIGHP"),
                         pattern = ".tif$",
                         recursive = TRUE,
                         full.names = TRUE)
sLowerSPRCHP <- sprc(sLowerRastHP)
sLowerMosaicHP <- mosaic(sLowerSPRCHP)
terra::writeRaster(sLowerMosaicHP, here('data', 'neon_aop', 'niwot_selected', 'S_LOWER',"HIGHP", 'south_lower_niwot_mosaic_HP.tif'))
rm(sLowerMosaicHP, sLowerSPRCHP)









classRast <- terra::rast(here('data', 'derived', 'southern_rockies_combined_class_topo.tif'))
neonAop <- st_read(here('data', 'NEON_AOP_FlightBoxes', 'NIWOT_AOP_Flightbox.shp'))
evtCodes <- read_csv(here('data', 'derived', 'evt_2020_newGroupsCodes_SRockies.csv'))


aoiClass <- classRast %>% terra::crop(neonAop, mask = TRUE)
plot(aoiClass)

set.seed(26913)
#Terra version is throwing an error for some reason
# potentialPlots <- aoiClass %>% spatSample(size = 5,
#                                              method = "stratified",
#                                              replace = FALSE,
#                                              na.rm = TRUE,
#                                              as.points = TRUE) %>%
#   st_as_sf()


potentialPlots <- aoiClass %>% raster::raster() %>% 
  raster::sampleStratified(size = 10, na.rm = TRUE, sp = TRUE) %>% 
  st_as_sf() %>%
  mutate(value = elevation) %>%
  select(-cell, -elevation) 

potentialPlots <- potentialPlots %>% mutate(elevation = case_when(value %/% 10000 %% 10 == 1 ~ "low",
                                                         value %/% 10000 %% 10 == 2 ~ "mid",
                                                         value %/% 10000 %% 10 == 3 ~ "high"),
                                  aspect = case_when(value %/% 1000 %% 10 == 1 ~ "north",
                                                      value %/% 1000 %% 10 == 2 ~ "east",
                                                      value %/% 1000 %% 10 == 3 ~ "south",
                                                      value %/% 1000 %% 10 == 4 ~ "west",),
                                  slope = case_when(value %/% 100 %% 10 == 1 ~ "flat/shallow",
                                                     value %/% 100 %% 10 == 2 ~ "moderate",
                                                     value %/% 100 %% 10 == 3 ~ "steep"),
                                  coverCode = value %% 100) %>%
  left_join(evtCodes, join_by(coverCode == NEW_CODE)) %>%
  na.omit()

