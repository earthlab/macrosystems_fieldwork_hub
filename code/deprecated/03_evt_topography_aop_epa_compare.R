### ### ### ### ### ### ### ### ### ### ### ###

#Landcover proportions in NEON AOP flight boxes & plots versus ecoregion they are in
#Based on a simplified EVT structure (from add_cats_landfire_evt.R)

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#Last updated: 6/19/23


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "concaveman", "mapview")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(terra)
library(sf)
library(here)
library(tidyverse)
library(concaveman)
library(mapview)


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


epsg <- "EPSG:26913"

# Create evt-topo combined layer ----

## Load data for combined evt-topo raster ----
evt <- terra::rast(here('data', 'derived', 'evt_2020_newGroups_SRockies.tif')) %>%
  `activeCat<-`("NEW_CODE")
crs(evt)
datatype(evt)
evtCodes <- read_csv(here('data', 'derived', 'evt_2020_newGroupsCodes_SRockies.csv'))

#DEM Data
demDats <- terra::rast(here('data', "topography_southern_rockies_EPSG_26913.tif"))



## Create layers to work with ----

#mapview(roads) + mapview(badUsfsRoads, color = list("red"), col.regions = list('red')) + mapview(allPplotsHp)

# Elevation - break into 3 elevation bands: 
#Front range at about 5400ft-6500ft
#Betasso at 6400, Schoonover at 6600
#MRS at 9500ft
#Gold hill at 8400
#Alpine: above 10500
#Subalpine: 8500/9000 - 12000
#Mid-elevation: 7000 - 9000
#Foothill shrublands: 6000 - 8500
#Low-elevation: sub-7500 (2286 m)
#Mid-elevation: 7500-9000 #shifted this lower end up a bit to provide more options in lower elevation band
#High-elevation: 9000+ (2743m)

elevationClass <- c(0, 2286, 10000,
                    2286, 2743, 20000,
                    2743, 4400, 30000)
elevationClass <- matrix(elevationClass, ncol = 3, byrow = TRUE)
elevation <- demDats$elevation %>% terra::classify(elevationClass)
plot(elevation)
terra::writeRaster(elevation, here('data', 'derived', 'southern_rockies_elevation_class.tif'),
                   datatype = "INT2U",
                   overwrite = TRUE)

# Aspect - break into strict NSEW (i.e. NOT including SE, SW, NW, NE).
#E = 67.5 - 122.5
#S = 157.5 - 202.5
#W = 147.5 - 292.5
#N = > 337.5 & < 22.5

#COMPLETE aspect breakdown for AOP analysis
aspectFullClass <- c(0, 45, 1000,
                     45, 135, 2000,
                     135, 225, 3000,
                     225, 315, 4000,
                     315, 360, 1000)
aspectFullClass <- matrix(aspectFullClass, ncol = 3, byrow = TRUE)
aspectFull <- demDats$aspect %>% terra::classify(aspectFullClass)
terra::writeRaster(aspectFull, here('data', 'derived', 'southern_rockies_aspect_FULL_class.tif'),
                   datatype = "INT2U",
                   overwrite = TRUE)


# Slope - break into 3 classes (some gaps between, want to ensure plot centroids are more squarely in the class)
#Steep: > 20 -> >21
#Mid: 10-20 -> 11-19
#Flat/shallow: <10 -> <9

#COMPLETE slope breakdown for AOP analysis

slopeFullClass <- c(0, 10, 100,
                    10, 20, 200,
                    20, 90, 300)
slopeFullClass <- matrix(slopeFullClass, ncol = 3, byrow = TRUE)
slopeFull <- demDats$slope %>% terra::classify(slopeFullClass)
terra::writeRaster(slopeFull, here('data', 'derived', 'southern_rockies_slope_FULL_class.tif'),
                   datatype = "INT2U",
                   overwrite = TRUE)

#Resample evt to match dem data
evtResampled <- evt %>% resample(demDats$slope, method = "near")

#change root evt index by classifying
evtClass <- cats(evtResampled)[[1]] %>% 
  select(Value, NEW_CODE) %>% 
  as.matrix() %>%
  as.numeric() %>%
  matrix(ncol = 2)
evtFinal <- evtResampled %>% terra::classify(evtClass)

terra::writeRaster(evtFinal, here('data', 'derived', 'southern_rockies_evt_final.tif'),
                   datatype = "INT2U",
                   overwrite = TRUE)
#evtFinal <- terra::rast(here('data', 'derived', 'southern_rockies_evt_final.tif'))

# Combine layers & export----
combinedFull <- elevation + slopeFull + aspectFull + evtFinal

#Export the combined data
terra::writeRaster(combinedFull, here('data', 'derived', 'southern_rockies_combined_FULL_class_topo.tif'),
                   datatype = "INT2U",
                   overwrite = TRUE)
#combinedFull <- terra::rast(here('data', 'derived', 'southern_rockies_combined_FULL_class_topo.tif'))


# Prep field data collected to include in analysis ----
field_aop <-sf::st_read(here('data', 'fieldwork', 'niwot_aop_macrosystems_data_2023_07_21_23.geojson')) %>%
  sf::st_transform(epsg)
field_plots_2022 <- sf::st_read(here('data', 'fieldwork', 'All_Macro_Plots_Summer_2022.shp')) %>%
  sf::st_transform(epsg) %>%
  mutate(type = "plot") %>% 
  select(type, geometry)
field_plots_2023 <- sf::st_read(here('data', 'fieldwork', 'macro_plots_2023.shp')) %>%
  sf::st_transform(epsg) %>%
  mutate(type = "plot") %>%
  filter(Completed == "Yes") %>% 
  select(type, geometry)
field_plots <- rbind(field_plots_2022, field_plots_2023)

#Create a bounding polygon around aop polygons that are close enough together
#Function to iteratively combine polygons into multipolygons by a set geographic distance between them
merge_within_distance <- function (polys, distance) {
  
  #Add identifiers
  ids <- seq(1:nrow(polys))
  polys <- polys %>% cbind(ids)
  
  #for each polygon, create the set
  for (i in 1:nrow(polys)) {
    p <- polys[i,] #get working polygon
    id <- p$ids #get ID of the working polygon
    associated <- p %>% st_is_within_distance(polys, dist = distance) #get the polygons within a distance of working poly
    associated <- associated[[1]] #get just the numbers
    #Change IDs of associated polygons to be the same as working polygon
    polys <- polys %>% mutate(workingID = case_when(ids %in% associated ~ id)) %>%
      mutate(ids = coalesce(workingID, ids))
  }
  
  #Turn into multipolygons by new IDs and then create final ids
  polys <- polys %>%
    group_by(ids) %>%
    summarize(geometry = st_union(geometry))
  
  return(polys)
}


field_aop_merged <- field_aop %>% merge_within_distance(20)
mapview(field_aop_merged)

concave_aop <- data.frame()
for(i in 1:nrow(field_aop_merged)) {
  mpoly <- field_aop_merged[i,]
  field_aop_merged_pts <- mpoly %>% 
    st_coordinates() %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y"))
  st_crs(field_aop_merged_pts) <- epsg
  concav <- concaveman::concaveman(field_aop_merged_pts)
  concave_aop <- concave_aop %>% rbind(concav)
}
concave_aop <- concave_aop %>% 
  mutate(type = "digitized_polygons")

mapview(concave_aop)

all_field <- concave_aop %>% 
  rbind(field_plots %>% 
          select(geometry, type) 
        %>% rename(polygons = geometry))



# Perform aop analysis ----

neonBasePlots <- sf::st_read(here('data', 'All_NEON_TOS_Plots_V9', "All_NEON_TOS_Plot_Polygons_V9.shp")) %>%
  dplyr::filter(subtype == "basePlot") %>%
  sf::st_transform(epsg)

aopFlightBoxes <- sf::st_read(here('data', 'NEON_AOP_FlightBoxes','AOP_flightboxesAllSites.shp')) %>%
    sf::st_transform(epsg)

epaLvl3 <- sf::st_read(here('data', 'EPA_Ecoregions', 'NA_CEC_Eco_Level3', 'NA_CEC_Eco_Level3.shp'))  %>%
  sf::st_transform(epsg)

#A function to subset NEON base plots and AOP flight polygons to a vector of given siteIDs
#and also add in the epa ecoregion that they are found in
pullSites <- function(sites) {
  basePlotsPulled <- neonBasePlots %>% dplyr::filter(siteID %in% sites)
  aopFlightBoxesPulled <- aopFlightBoxes %>% dplyr::filter(siteID %in% sites)
  epaEcoRegPulled <- epaLvl3 %>% sf::st_filter(aopFlightBoxesPulled)
  pulled <- list(basePlotsPulled, aopFlightBoxesPulled, epaEcoRegPulled) %>%
    `names<-`(c("BasePlots", "FlightBoxes", "EcoRegions"))
  return(pulled)
}


#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped and masked to vector
careful.clip.mask <- function(vector, raster) {
  if (sf::st_crs(vector) != terra::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
    print("Projecting vector")
    vector <- sf::st_transform(vector, terra::crs(raster)) 
  } else {
    print("Vector already in raster CRS")
  }
  print("Clipping")
  r <- terra::crop(raster,
                   vector,
                   mask = TRUE) #crop & mask
  return(r)
}

#Function to analyze EVT + topo within the given areas of interest

#Function to get frequencies of cover within AOIs
#aoi = output from pullSites, i.e. a list of 
#1) base plot polygons, 2) AOP flight polygons, 3) EcoRegion polygons, 4) field polygons
#regionCover = FULL evt + topo raster
get.freqs <- function(aoi, regionCover) {
  print("ClipMasking")
  freqs <- aoi %>% 
    lapply(function(x) 
      careful.clip.mask(x, regionCover) %>% 
        freq() %>%
        select(-layer)) %>%
    `names<-`(c("BasePlotFreqs", "FlightBoxFreqs", "EcoRegionFreqs", "FieldFreqs"))
  out <- freqs$EcoRegionFreqs %>%
    rename(EcoRegionCount = count) %>%
    left_join(freqs$FlightBoxFreqs) %>%
    rename(FlightBoxCount = count) %>%
    left_join(freqs$BasePlotFreqs) %>%
    rename(BasePlotCount = count) %>%
    left_join(freqs$FieldFreqs) %>%
    rename(FieldCount = count)
  return(out)
}

add.dats <- function(freqs) {
  out <- freqs %>% dplyr::mutate(elevation = case_when(value %/% 10000 %% 10 == 1 ~ "low",
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
    left_join(evtCodes, join_by(coverCode == NEW_CODE))
  
  tots <- c(sum(out$EcoRegionCount),
            sum(out$FlightBoxCount, na.rm = TRUE),
            sum(out$BasePlotCount, na.rm = TRUE),
            sum(out$FieldCount, na.rm = TRUE)) %>% 
    `names<-`(c('EcoTot', 'FlightTot', 'BaseTot', 'FieldTot'))
  
  out <- out %>% dplyr::mutate(EcoRegionPerc = 100 * (EcoRegionCount / tots[1]),
                            FlightBoxPerc = 100 * (FlightBoxCount / tots[2]),
                            BasePlotPerc = 100 * (BasePlotCount / tots[3]),
                            FieldPerc = 100 * (FieldCount / tots[4])) %>%
    dplyr::filter(NEW_GP_N != "Developed" &
                    NEW_GP_N != "Open Water" &
                    NEW_GP_N != "Introduced" &
                    NEW_GP_N != "Agricultural" &
                    NEW_GP_N != "Alpine" &
                    NEW_GP_N != "Sparse Vegetation") %>%
    filter(!is.na(aspect) & !is.na(slope)) %>%
    dplyr::arrange(desc(EcoRegionCount)) 
  
  return(out)
}


sRockies <- pullSites(c("NIWO", "RMNP", "COMO"))
sRockies[[4]] <- all_field
names(sRockies)[4] <- "Field"

sRockiesData <- sRockies %>% 
  get.freqs(combinedFull) %>% 
  add.dats()

# Get AOP priorities ----

#A function that takes in a filtered set of the data and a name (e.g. aop_remaining_priority_raster)
#And writes out a raster showing remaining areas to sample and their priorities (by % of ecoregion cover)
create.remaining.priorities.raster <- function(remainFiltered, nm) {
  #Get cover values
  remainCovers <- remainFiltered$value
  #Turn into raster
  remainingRast <- ifel(combinedFull %in% remainCovers, combinedFull, NA)
  #Create classification matrix
  remainClassify <- remainFiltered %>%
    select(value, EcoRegionPerc) %>%
    as.matrix()
  #Reclassify to have priorities instead of topo-cover IDs
  print("Reclassifying to priorities")
  remainingPriorityRast <- remainingRast %>%
    terra::classify(remainClassify)
  #Write out
  print("Writing raster")
  terra::writeRaster(remainingPriorityRast,
                     here('data', 'derived', 
                          paste(nm, "_", Sys.Date(), '.tif', sep = "")),
                     overwrite = TRUE)
}

#Filter to aop remaining areas of interest
aop_remaining <- sRockiesData %>% filter(FlightBoxCount >= 5 &
                                     is.na(BasePlotCount) &
                                     is.na(FieldCount))

create.remaining.priorities.raster(aop_remaining, "aop_remaining_priority_raster")

# Get non-AOP priorities ----
non_aop_remaining <- sRockiesData %>% filter((FlightBoxCount <5 | is.na(FlightBoxCount)) &
                                               is.na(BasePlotCount) &
                                               is.na(FieldCount))

create.remaining.priorities.raster(non_aop_remaining, "non_aop_remaining_priority_raster")




# Run for 'hypothetical field visits'

sRockiesHyp <- pullSites(c("NIWO", "RMNP", "COMO"))

hypothetical <- sf::st_read(here::here('data', 'fieldwork', 'hypothetical.shp')) %>% 
  sf::st_transform(epsg) %>%
  mutate(type = "hypothetical") %>%
  select(-id) %>%
  rename(polygons = geometry)

all_field_plus_hyp <- all_field %>% 
  rbind(hypothetical)

sRockiesHyp[[4]] <- all_field_plus_hyp
names(sRockiesHyp)[4] <- "Field"

sRockiesDataHyp <- sRockiesHyp %>% 
  get.freqs(combinedFull) %>% 
  add.dats()

aop_remaining_hyp <- sRockiesDataHyp %>% filter(FlightBoxCount >= 5 &
                                           is.na(BasePlotCount) &
                                           is.na(FieldCount))

create.remaining.priorities.raster(aop_remaining_hyp, "aop_remaining_hyp_priority_raster")


non_aop_remaining_hyp <- sRockiesDataHyp %>% filter(FlightBoxCount <5 | is.na(FlightBoxCount) &
                                                  is.na(BasePlotCount) &
                                                  is.na(FieldCount))

create.remaining.priorities.raster(non_aop_remaining_hyp, "non_aop_remaining_hyp_priority_raster")



# Do run showing field mapping works well

niwo <- pullSites(c("NIWO"))
niwo[[4]] <- all_field
names(niwo)[4] <- "Field"

niwoD <- niwo %>% 
  get.freqs(combinedFull) %>% 
  add.dats()

niwoRemain <- niwoD %>% filter(FlightBoxCount >= 5 &
                                           is.na(BasePlotCount))

niwoRemainWField <- niwoD %>% filter(FlightBoxCount >= 5 &
                                       is.na(BasePlotCount) &
                                       is.na(FieldCount))

create.remaining.priorities.raster(niwoRemain, "niwo_remaining_no_field_priority_raster")
create.remaining.priorities.raster(niwoRemainWField, "niwo_remaining_with_field_priority_raster")


