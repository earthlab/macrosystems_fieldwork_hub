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

oli <- read_csv(here('data', 'spectral_library', 'cleaned_spectra_OLI.csv'))
olipft <- read_csv(here('data', 'spectral_library', 'cleaned_spectra_OLI_PFT.csv'))

niwo <- olipft %>% filter(grepl("NIWO", namedLocation, ignore.case = TRUE))
rmnp <- olipft %>% filter(grepl("RMNP", namedLocation, ignore.case = TRUE))
cper <- olipft %>% filter(grepl("CPER", namedLocation, ignore.case = TRUE))
moab <- olipft %>% filter(grepl("MOAB", namedLocation, ignore.case = TRUE))
yell <- olipft %>% filter(grepl("YELL", namedLocation, ignore.case = TRUE))


fieldwork <- read_csv(here('data', 'fieldwork', 'neon_aop_6_15_23.csv'))
evergreen <- fieldwork %>% filter(cover_category == "Evergreen")

