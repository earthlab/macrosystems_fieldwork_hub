### ### ### ### ### ### ### ### ### ### ### ###

#Landcover proportions in NEON AOP flight boxes & plots versus ecoregion they are in
#Based on a simplified GAP CONUS class structure

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#3/16/23


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
percThresh <- 2

## Load data ----
gapConusMiddleRockies <- terra::rast(here('data', 'GapConusCover2011_MiddleRockies_EPSG_26912_GEE.tif'))
gapConusSouthernRockies <- terra::rast(here('data', 'GapConusCover2011_SouthernRockies_EPSG_32613_GEE.tif'))
gapConusCascades <- terra::rast(here('data', 'GapConusCover2011_Cascades_EPSG_26910_GEE.tif'))

aopYELL <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes','YELL_NEON_AOP_FlightBox.shp'))
aopNIWO <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes','NIWOT_AOP_Flightbox.shp'))
aopWREF <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes' ,'WREF_AOP_FlightBox.shp'))

neonBasePlots <- sf::st_read(here('data', 'All_NEON_TOS_Plots_V9', "All_NEON_TOS_Plot_Polygons_V9.shp")) %>%
  dplyr::filter(subtype == "basePlot")

niwoPlots <- neonBasePlots %>% dplyr::filter(siteID == "NIWO")
yellPlots <- neonBasePlots %>% dplyr::filter(siteID == "YELL")
wrefPlots <- neonBasePlots %>% dplyr::filter(siteID == "WREF")

gapConusCodes <- read.csv(here('data', 'gap_conus_2011_codes.csv'))
gapConusLookup <- read.csv(here('data', 'gap_conus_lookup_codes.csv'))


#fxtbl <- gapConusLookup %>% select(-Description) %>% left_join(select(gapConusCodes, -Color), by = 'GAP_CONUS_2011_Code') %>% relocate(Category, .after = last_col())
#write.csv(fxtbl, 'data/gap_conus_lookup_codes.csv')


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
  
  #Join together and join codes
  freqs <- regionFreq %>%
    full_join(aoiFreq, by = c("value")) %>%
    mutate_if(is.numeric,coalesce,0) %>% #remove NAs
    filter(value != 0) %>% #remove background
    left_join(gapConusCodes, by = c("value" = "GAP_CONUS_2011_Code")) #add code names
  
  names(freqs) <- c("gapCode",
                    "regionCount",
                    "aoiCount",
                    "Color",
                    "Description")
  
  #Add percentages & diff
  print("Calculating Percentages")
  freqs <- freqs %>% mutate(regionPerc = 100*(regionCount / sum(freqs$regionCount))) %>%
    mutate(aoiPerc = 100*(aoiCount / sum(freqs$aoiCount))) %>%
    mutate(diffInPerc = regionPerc - aoiPerc)

  #Arrange
  freqs <- freqs %>%
    arrange(desc(diffInPerc)) #arrange descending

  #Add in categories to simplify later
  freqs <- freqs %>%
    left_join(gapConusLookup, by = c("gapCode" = "GAP_CONUS_2011_Code"), Description) %>%
    select(-Description.y) %>%
    rename(Description = Description.x)

  return(freqs)
}


### ### ### ### ### ### ### ### ### ### ### ###

# PERFORM LANDCOVER ANALYSIS ----

## Run functions for each area ----

#Run function for Middle Rockies AOP
lcAnalysisYellowstone <- analyze.landcover(aoi = aopYELL, regionCover = gapConusMiddleRockies) %>%
  rename(AOPperc = aoiPerc, AOPdiffInPerc = diffInPerc)

#Run function for Middle Rockies Base Plots
lcAnalysisYPlots <- analyze.landcover(aoi = yellPlots, regionCover = gapConusMiddleRockies) %>%
  rename(BasePlotPerc = aoiPerc, BasePlotDiffInPerc = diffInPerc)

#Run function for Southern Rockies AOP
lcAnalysisNiwot <- analyze.landcover(aoi = aopNIWO, regionCover = gapConusSouthernRockies) %>%
  rename(AOPperc = aoiPerc, AOPdiffInPerc = diffInPerc)

#Run function for Southern Rockies Base Plots
lcAnalysisNPlots <- analyze.landcover(aoi = niwoPlots, regionCover = gapConusSouthernRockies) %>%
  rename(BasePlotPerc = aoiPerc, BasePlotDiffInPerc = diffInPerc)

#Run function for Cascades AOP
lcAnalysisWref <- analyze.landcover(aoi = aopWREF, regionCover = gapConusCascades) %>%
  rename(AOPperc = aoiPerc, AOPdiffInPerc = diffInPerc)

#Run function for Cascades Base Plots
lcAnalysisWPlots <- analyze.landcover(aoi = wrefPlots, regionCover = gapConusCascades) %>%
  rename(BasePlotPerc = aoiPerc, BasePlotDiffInPerc = diffInPerc)


## Combine datasets and clean up ----
combine.aop.plots <- function(aopAnalysis, plotAnalysis) {
  x <- full_join(aopAnalysis, select(plotAnalysis, gapCode, BasePlotPerc, BasePlotDiffInPerc)) %>%
    select(-regionCount, -aoiCount, -Color)
  return(x)
}

lcMRockies <- combine.aop.plots(lcAnalysisYellowstone, lcAnalysisYPlots)
lcSRockies <- combine.aop.plots(lcAnalysisNiwot, lcAnalysisNPlots)
lcCascades <- combine.aop.plots(lcAnalysisWref, lcAnalysisWPlots)

## Summarize data ----

#Function to summarize
summarize.combined <- function(analysis) {
  x <- analysis %>%
    group_by(Category) %>%
    summarize(regionPercS = sum(regionPerc),
              aopPercS = sum(AOPperc),
              plotPerc = sum(BasePlotPerc)) %>%
    arrange(desc(regionPercS))
  return(x)
}

lcMRockiesSimple <- summarize.combined(lcMRockies) %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)

lcSRockiesSimple <- summarize.combined(lcSRockies) %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)

lcCascadesSimple <- summarize.combined(lcCascades) %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)


### ### ### ### ### ### ### ### ### ### ### ###

# FILTER TO FOREST ONLY ----

#Function to filter to only include forest classes
#Keywords: Forest, Woodland, Savanna
#Takes in a frequency table from analyze.landcover
filter.to.forest <- function(freqs) {
  print("Filtering")
  freqsForest <- freqs %>% filter(grepl("Forest", Description, fixed = TRUE) | 
                                    grepl("Woodland", Description, fixed = TRUE) |
                                    grepl("Savanna", Description, fixed = TRUE))
  
  freqsForest <- freqsForest %>%
    arrange(desc(regionPerc)) #arrange descending
  
  return(freqsForest)
}

lcMRockiesSimpleForest <- lcMRockies %>%
  filter.to.forest() %>%
  summarize.combined() %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)

lcSRockiesSimpleForest <- lcSRockies %>%
  filter.to.forest() %>%
  summarize.combined() %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)

lcCascadesSimpleForest <- lcCascades %>%
  filter.to.forest() %>%
  summarize.combined() %>%
  filter(regionPercS > percThresh) %>%
  gather(key = percGroup, value = percent, regionPercS:plotPerc)


### ### ### ### ### ### ### ### ### ### ### ###

# VISUALIZE RESULTS ----

viz <- function(analysis, name) {
  ggplot(data = analysis, aes(fill = percGroup, x = Category, y = percent)) +
    geom_col(position = "dodge") + 
    ggtitle(paste(name, "Landcover Percentages", sep = " "))
  ggsave(here('figs', paste(name, '.png', sep ="")), width = 13, height = 7, units = "in")
}

viz(lcMRockiesSimple, "Middle_Rockies")
viz(lcSRockiesSimple, "Southern_Rockies")
viz(lcCascadesSimple, "Cascades")

viz(lcMRockiesSimpleForest, "Middle_Rockies_Forest")
viz(lcSRockiesSimpleForest, "Southern_Rockies_Forest")
viz(lcCascadesSimpleForest, "Cascades_Forest")




