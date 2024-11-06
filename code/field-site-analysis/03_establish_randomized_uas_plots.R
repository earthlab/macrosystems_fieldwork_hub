# This script randomizes site selection for the Macrosystems project based on field sampling rasters
# created by script 02


#Tyler L. McIntosh

# Setup ----
rm(list = ls())

if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("code", "functions.R"))

install_and_load_packages(
  package_list = c(
    "here",
    "terra",
    "sf",
    "tidyverse",
    "glue",
    "mapview",
    "raster",
    "tigris",
    "httr",
    "osmdata"),
  auto_install = "y"
)

# Set up necessary data directories
dir_ensure(here::here("data"))
dir_raw <- here::here("data/raw")
dir_derived <- here::here("data/derived")
dir_field_rasters <- here::here(dir_derived, "field_planning_rasters")
dir_ensure(dir_raw)
dir_ensure(dir_derived)
dir_ensure(dir_field_rasters)

# Set global parameters ----
distFromRoadsMax <- 250
distFromRoadsMin <- 70 #plot diagonal is 127.28m. Round up to 130 & /2 = 65m ensures that road does not pass through plot. Add 5m buffer = 70
epsg <- "EPSG:5070" #Albers equal area

# Data access

test <- tigris::counties() |>
  dplyr::filter(NAME == "Boulder")

ranger <- access_usfs_ranger_districts()

usfs <- access_us_sma(dir_path = dir_raw,
                        layer = "SurfaceMgtAgy_USFS")
access_us_sma_helper_show_layers(dir_raw)
blm <- access_us_sma(dir_path = dir_raw,
                       layer = "SurfaceMgtAgy_BLM")

epa_l3 <- access_data_epa_l3_ecoregions_vsi()



## Bend area ----
bendRanger <- ranger |>
  dplyr::filter(DISTRICTNA %in% c("Bend/Fort Rock Ranger District", "Sisters Ranger District")) |>
  sf::st_buffer(-400)

sistersRanger <- ranger |>
  dplyr::filter(DISTRICTNA %in% c("Sisters Ranger District")) |>
  sf::st_buffer(-400)

crescentRanger <- ranger |>
  dplyr::filter(DISTRICTNA %in% c("Crescent Ranger District")) |>
  sf::st_buffer(-400)

chemultRanger <- ranger |>
  dplyr::filter(DISTRICTNA %in% c("Chemult Ranger District")) |>
  sf::st_buffer(-400)

hoodRiverRanger <- ranger |>
  dplyr::filter(DISTRICTNA %in% c("Hood River Ranger District")) |>
  sf::st_buffer(-400)

cascadesCore <- epa_l3 |>
  dplyr::filter(US_L3NAME == "Cascades") %>%
  dplyr::mutate(id = seq(1:nrow(.))) |>
  dplyr::filter(id == 4)

evt5winNotCascadeAOP <- terra::rast(here::here('data', 'field_plan_sets', 'cascadesCoreBuff5Window_0-001_0-1', 'cascadesCoreBuff5Window_0-001_0-1_not_in_aop.tif')) |>
  add.evt.cats()

x <- create_operating_area(bendRanger, cascadesCore)


ecoRegion <- cascadesCore
ranger <- bendRanger

epsg <- "EPSG:5070" #Albers equal area

ranger <- ranger |>
  sf::st_transform(epsg)

ecoRegion <- ecoRegion |>
  sf::st_transform(epsg)

reasonableAreasOfAccess <- sf::st_read(here::here("data/manual/reasonable_areas_of_access.gpkg")) |>  #these are manually created areas of access. For YELL it is from create_yell_neon_aoi.R
  sf::st_transform(epsg) |>
  dplyr::mutate(grp = 1) |>
  dplyr::group_by(grp) |>
  dplyr::summarise(geom = st_union(geom)) |>
  dplyr::ungroup()

#Wilderness & WSA
wildernessAll <- access_us_wilderness(dest_path = here::here('data', 'raw', 'wild.gpkg')) |> 
  sf::st_transform(epsg) |>
  sf::st_buffer(400) #USFS standard is no drones w/in 300m of wilderness boundaries + 70, same reasoning as road buffer; add an extra 30m to be safe = 400

wilderness <- wildernessAll |>
  sf::st_filter(ranger) |>
  dplyr::mutate(group = 1) |>
  dplyr::group_by(group) |>
  dplyr::summarise(geom = st_union(geom)) |>
  dplyr::ungroup()

wsaAll <- access_us_wilderness_study_areas(dest_path = here::here('data', 'raw', 'wsa.gpkg')) |>
  sf::st_transform(epsg) |>
  sf::st_buffer(400) #USFS standard is no drones w/in 300m of wilderness boundaries + 70, same reasoning as road buffer; add an extra 30m to be safe = 400

wsa <- wsaAll |>
  sf::st_filter(ranger) |>
  dplyr::mutate(group = 1) |>
  dplyr::group_by(group) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()

genOpArea <- ecoRegion |>
  sf::st_buffer(-50) |>
  sf::st_intersection(ranger) |>
  dplyr::mutate(group = 1) |>
  dplyr::group_by(group) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup() |>
  sf::st_intersection(reasonableAreasOfAccess)

if(nrow(wilderness) > 0) {
  genOpArea <- genOpArea |>
    sf::st_difference(wilderness)
}
if(nrow(wsa) > 0) {
  genOpArea <- genOpArea |>
    sf::st_difference(wsa)
}  







