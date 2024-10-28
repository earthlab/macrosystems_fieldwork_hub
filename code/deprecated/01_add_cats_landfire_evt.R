### ### ### ### ### ### ### ### ### ### ### ###

#Add new categories and codes to Landfire Existing Vegetation Type
#Output raster layers based on a number of classification schemes

#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#Last updated: 4/15/2023


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

outEPSG = "EPSG:26913"


## Load data ----
evtDir <- here('data', 'existing-vegetation-type', 'LF2020_EVT_220_CONUS') #EVT directory


evtFile <- list.files(evtDir,
                      pattern = ".tif$",
                      recursive = TRUE,
                      full.names = TRUE)
evtCsv <- list.files(evtDir,
                     pattern = ".csv$",
                     recursive = TRUE,
                     full.names = TRUE)

evt <- terra::rast(evtFile)
evtD <- read.csv(evtCsv)
#aoi <- sf::st_read(here::here('data', 'EPA_Ecoregions', 'EPA_lvl3_SRockies_epsg32613.shp'))

require(glue)
require(sf)

epa_l3 <- glue::glue(
  "/vsizip/vsicurl/", #magic remote connection
  "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", #copied link to download location
  "/us_eco_l3.shp") |> #path inside zip file
  sf::st_read()

aoi <- epa_l3 |>
  dplyr::filter(US_L3NAME == "Middle Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()
aoiNm <- 'MRockies'



# Safe clip ----

#Function to clip a raster to a vector, ensuring in same projection
careful.clip <- function(raster, vector) {
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

evtClip <- evt %>% careful.clip(aoi)


# Create new groups ----
newGroups <- unique(evtD$EVT_GP_N) %>% #get unique veg group names
  as.data.frame() %>%
  `names<-`(c("EVT_GP_N")) %>%
  #Note that the case_when statement will evaluate UNTIL a match is found, at which point it terminates
  mutate(NEW_GP_N = case_when(grepl(pattern = 'Introduced', EVT_GP_N, ignore.case = TRUE) ~ "Introduced",
                              grepl(pattern = "Agricult", EVT_GP_N, ignore.case = TRUE) ~ "Agricultural",
                              grepl(pattern = "Developed|quarries", EVT_GP_N, ignore.case = TRUE) ~ "Developed",
                              grepl(pattern = "Wetland|Marsh", EVT_GP_N, ignore.case = TRUE) ~ "Wetland",
                              grepl(pattern = 'riparian', EVT_GP_N, ignore.case = TRUE) ~ "Riparian",
                              grepl(pattern = "prairie|grassland|meadow|tundra", EVT_GP_N, ignore.case = TRUE) ~ "PrairieGrasslandMeadow",
                              grepl(pattern = "shrub|scrub", EVT_GP_N, ignore.case = TRUE) ~ "ShrubScrub",
                              grepl(pattern = 'Douglas-fir*', EVT_GP_N, ignore.case = TRUE) ~ "DouglasFir",
                              grepl(pattern = "snow|barren", EVT_GP_N, ignore.case = TRUE) ~ "Alpine",
                              TRUE ~ EVT_GP_N))
newCodes <- cbind(unique(newGroups$NEW_GP_N),
                  seq(1:length(unique(newGroups$NEW_GP_N)))) %>% #create new codes for new gorups
  as.data.frame() %>%
  `names<-`(c("NEW_GP_N", "NEW_CODE"))
newGroups <- newGroups %>% left_join(newCodes) #join together

# Create new cats ----
#This raster is composed of factors. We can add categories to it and set the active 'cat'
#Join "NEW_GP_N" & "NEW_CODE" to cats
newCats <- terra::cats(evtClip)[[1]] %>%
  left_join(newGroups)

newEvtClip <- evtClip %>%
  `levels<-`(newCats) %>%
  `activeCat<-`('NEW_GP_N') %>%
  project(outEPSG)

plot(newEvtClip)
freq(newEvtClip)



t <- newCats %>% filter(grepl('riparian', NEW_GP_N, ignore.case = TRUE))




#Write out ----
terra::writeRaster(newEvtClip,
                   here('data', 'derived', paste("evt_2020_newGroups_", aoiNm, ".tif", sep = "")),
                   overwrite = TRUE,
                   datatype = "INT2U")

write_csv(newCodes, here('data', 'derived', paste("evt_2020_newGroupsCodes_", aoiNm, ".csv", sep = "")))




#Test written data
x <- terra::rast(here('data', 'derived', paste("evt_2020_newGroups_", aoiNm, ".tif", sep = "")))
activeCat(x) <- 'NEW_GP_N'
plot(x)





#A function to add a column of new unique codes (newColNm) to a dataset based on an input column name (colNm)
create.new.codeset <- function(datFrame, colNm, newColNm) {
  colVals <- datFrame %>%
    select(!!colNm) %>%
    unique()
  newCodes <- seq(1:nrow(colVals))
  codeSet <- cbind(colVals, newCodes) %>% as.data.frame() %>%
    `names<-`(c(colNm, newColNm))
  datFrame <- datFrame %>% left_join(codeSet)
  return(datFrame)
}

# A function to take in landfire EVT raster and CSV data and a given column name,
# and then re-classify the raster according to unique codes for the column
# including RGB
# uses create.new.codesset()
re.class.evt <- function(evtRast, colNm, evtData) {
  classifyM <- evtData %>%
    select(VALUE, !!colNm) %>%
    as.matrix()
  newRast <- evtRast %>% terra::classify(classifyM)
  return(newRast)
}

evt_with_codes <- create.new.codeset(evtD, "EVT_CLASS", "EVT_CLASS_CODE")
evt_class_classified <- re.class.evt(evtRast = evtClip, colNm = "EVT_CLASS_CODE", evtData = evt_with_codes)
terra::plot(evt_class_classified)




# 
# # A function to write a QGIS color map for EVT
# # based on a column of names and a column of values in the raster
# 
# #THIS DOES NOT WORK YET!! WASTE OF TIME
# evt_qgis_colormap <- function(evtData, nameCol, valueCol) {
#   #Get distinct value set
#   colormap <- evtData %>%
#     select(!!nameCol, !!valueCol) %>%
#     distinct()
#   #names
#   nms <- colormap[[nameCol]]
#     
#   rgb <- data.frame() #empty
#   #Filter to each name and take the first row, use RGB from this
#   for (nm in nms) {
#     print(nm)
#     set <- evtData %>%
#       filter(!!nameCol == nm)
#   rgb <- rbind(rgb, set)
#   print(set)
#   }
#   colormap <- cbind(colormap, rgb)
# 
#   return(rgb)
# }
# 
# newFrame <- create.new.codeset(datFrame = evtD,
#                                    colNm = "EVT_CLASS",
#                                    newColNm = "EVT_CLASS_CODE")
# test <- evt_qgis_colormap(newFrame, "EVT_CLASS", "EVT_CLASS_CODE")






t <- cats(evtClip)[[1]]
riparian <- t %>% filter(grepl(pattern = 'riparian', EVT_NAME, ignore.case = TRUE))  
PGM <- t %>% filter(grepl(pattern = "prairie|grassland|meadow", EVT_GP_N, ignore.case = TRUE))
shrub <- t %>% filter(grepl(pattern = "scrub|shrub", EVT_GP_N, ignore.case = TRUE))
alpine <- t %>% filter(grepl(pattern = "snow|tundra|alpine", EVT_GP_N, ignore.case = TRUE))
rMount <- t %>% filter(grepl(pattern = "Rocky Mountain", EVT_NAME, ignore.case = TRUE))


unique(t$EVT_LF)
unique(t$EVT_PHYS)
unique(t$EVT_ORDER)
unique(t$EVT_CLASS)
unique(t$EVT_SBCLS)
unique(cats(x)[[1]] %>% select(NEW_GP_N))
