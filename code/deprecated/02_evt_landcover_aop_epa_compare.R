### ### ### ### ### ### ### ### ### ### ### ###

#Landcover proportions in NEON AOP flight boxes & plots versus ecoregion they are in
#Based on a simplified EVT structure (from add_cats_landfire_evt.R)

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#Last updated: 4/16/23


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

#Set threshold percentage: once classes have been combined,
#combined classes below percThresh percent of the region will be removed
percThresh <- 0

## Load data ----
#gapConusMiddleRockies <- terra::rast(here('data', 'GapConusCover2011_MiddleRockies_EPSG_26912_GEE.tif'))
evtSouthernRockies <- terra::rast(here('data', 'derived', 'evt_2020_newGroups_SRockies.tif')) %>%
  `activeCat<-`("NEW_GP_N")
#gapConusCascades <- terra::rast(here('data', 'GapConusCover2011_Cascades_EPSG_26910_GEE.tif'))
evtMiddleRockies <- terra::rast(here('data', 'derived', 'evt_2020_newGroups_MRockies.tif')) %>%
  `activeCat<-`("NEW_GP_N")


aopYELL <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes','YELL_NEON_AOP_FlightBox.shp'))
aopNIWO <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes','NIWOT_AOP_Flightbox.shp'))
#aopWREF <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes' ,'WREF_AOP_FlightBox.shp'))

neonBasePlots <- sf::st_read(here('data', 'All_NEON_TOS_Plots_V9', "All_NEON_TOS_Plot_Polygons_V9.shp")) %>%
  dplyr::filter(subtype == "basePlot")

niwoPlots <- neonBasePlots %>% dplyr::filter(siteID == "NIWO")
yellPlots <- neonBasePlots %>% dplyr::filter(siteID == "YELL")
#wrefPlots <- neonBasePlots %>% dplyr::filter(siteID == "WREF")


### ### ### ### ### ### ### ### ### ### ### ###

# KEY FUNCTIONS ----

## Function to clip and mask
clipmask.raster.to.vector <- function(raster, vector, setCRS) {
  if (crs(vector) != setCRS) {
    print("Projecting vector")
    vector <- st_transform(vector, setCRS) 
    print("Vector projected")
  } else {
    print("Vector already in chosen CRS")
  }
  if (crs(raster) != setCRS) {
    print("Projecting raster")
    raster <- project(raster, setCRS)
    print("Raster projected")
  } else {
    print("Raster already in chosen CRS")
  }
  if(ext(raster) != ext(vector)) {
    print("Cropping raster to vector")
    raster <- crop(raster, ext(vector))
    print("Cropped")
  } else {
    print("Extents already match")
  }
  print("Masking raster to vector")
  rMask <- mask(raster, vector)
  print("Masked")
  print("Done")
  return(rMask)
}


## Function to analyze landcover comparisons between AOI and a larger region-
#aoi - the smaller region of interest
#regionCover - GAP conus code raster for a region
#Uses clipmask.raster.to.vector
analyze.landcover <- function(aoi, regionCover) {
  
  #Run clip + mask function on aop region
  rasterCRS <- crs(regionCover)
  gapAOI <- clipmask.raster.to.vector(regionCover, aoi, rasterCRS)
  
  #Get frequencies
  print("Getting frequencies")
  aoiFreq <- freq(gapAOI) %>% select(-layer)
  regionFreq <- freq(regionCover) %>% select(-layer)
  
  ####CODE CHANGED TO MAKE WORK W/ EVT: need to group+summarise by value
  
  #This function takes in a table returned from terra::freq()
  #If there are more values in the table than unique values then the function will condense it
  #And replace 'counts' with a sum of all counts for the same value name
  condense.freq.groups <- function(freqDats) {
    if (length(unique(freqDats$value)) == length(freqDats$value)) {
      print('Frequency table is already correct')
      return(freqDats)
    } else {
      print('Condensing frequency table')
      freqDats <- freqDats %>%
        group_by(value) %>%
        summarise(count = sum(count)) %>%
        ungroup()
      return(freqDats)
    }
  }
  
  aoiFreq <- aoiFreq %>% condense.freq.groups()
  regionFreq <- regionFreq %>% condense.freq.groups()
  
  #Join together and join codes
  freqs <- regionFreq %>%
    full_join(aoiFreq, by = c("value")) %>%
    mutate_if(is.numeric,coalesce,0) %>% #remove NAs
    filter(value != 0) #remove background
  
  names(freqs) <- c("NEW_EVT_GP_N",
                    "regionCount",
                    "aoiCount")
  
  #Add percentages & diff
  print("Calculating Percentages")
  freqs <- freqs %>% mutate(regionPerc = 100*(regionCount / sum(freqs$regionCount))) %>%
    mutate(aoiPerc = 100*(aoiCount / sum(freqs$aoiCount))) %>%
    mutate(diffInPerc = regionPerc - aoiPerc)
  
  #Arrange
  freqs <- freqs %>%
    arrange(desc(diffInPerc)) #arrange descending
  
  return(freqs)
}




### ### ### ### ### ### ### ### ### ### ### ###

# PERFORM LANDCOVER ANALYSIS ----

## Run functions for each area ----

#Run function for Southern Rockies AOP
lcAnalysisNiwot <- analyze.landcover(aoi = aopNIWO, regionCover = evtSouthernRockies) %>%
  rename(AOPperc = aoiPerc, AOPdiffInPerc = diffInPerc)

#Run function for Southern Rockies Base Plots
lcAnalysisNPlots <- analyze.landcover(aoi = niwoPlots, regionCover = evtSouthernRockies) %>%
  rename(BasePlotPerc = aoiPerc, BasePlotDiffInPerc = diffInPerc)

#Run function for Middle Rockies & plots
lcAnalysisYell <- analyze.landcover(aoi = aopYELL, regionCover = evtMiddleRockies) %>%
  rename(AOPperc = aoiPerc, AOPdiffInPerc = diffInPerc)
lcAnalysisYPlots <- analyze.landcover(aoi = yellPlots, regionCover = evtMiddleRockies) %>%
  rename(BasePlotPerc = aoiPerc, BasePlotDiffInPerc = diffInPerc)

## Combine datasets and clean up ----
combine.aop.plots <- function(aopAnalysis, plotAnalysis) {
  x <- full_join(aopAnalysis, select(plotAnalysis, NEW_EVT_GP_N, BasePlotPerc, BasePlotDiffInPerc)) %>%
    select(-regionCount, -aoiCount)
  return(x)
}

lcSRockies <- combine.aop.plots(lcAnalysisNiwot, lcAnalysisNPlots)
lcMRockies <- combine.aop.plots(lcAnalysisYell, lcAnalysisYPlots)


## Summarize data ----

#Function to summarize & create long data
summarize.combined <- function(analysis) {
  x <- analysis %>%
    group_by(NEW_EVT_GP_N) %>%
    summarize(regionPercS = sum(regionPerc),
              aopPercS = sum(AOPperc),
              plotPerc = sum(BasePlotPerc)) %>%
    arrange(desc(regionPercS))
  return(x)
}


lcSRockiesSimple <- summarize.combined(lcSRockies) %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)

lcMRockiesSimple <- summarize.combined(lcMRockies) %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)


### ### ### ### ### ### ### ### ### ### ### ###

# VISUALIZE RESULTS ----

viz <- function(analysis, name) {
  ggplot(data = analysis, aes(fill = percGroup, x = NEW_EVT_GP_N, y = percent)) +
    geom_col(position = "dodge") + 
    xlab("EVT Group") + ylab("Percent") +
    ggtitle(paste(name, "Landcover Percentages", sep = " ")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 7))
  ggsave(here('figs', paste(name, '.png', sep ="")), width = 20, height = 7, units = "in")
}

viz(lcSRockiesSimple, "Southern_Rockies_EVT")

viz(lcMRockiesSimple, "Middle_Rockies_EVT")




