# A script to see what EVT classes the AOP and UAS polygons from summer 2023 were in
# Tyler L McIntosh, Earth Lab, CU Boulder
# Last updated: 5/22/24

rm(list=ls()) #Ensure empty workspace


library(sf)
library(terra)
library(mapview)
library(tidyverse)
library(here)
library(glue)

epsg <- "EPSG:5070" #Albers equal area

# LOAD DATA
uas <- sf::st_read(here::here("data/fieldwork/uas_polygons_2023_12_13_23_analysis_ready_half_diam.gpkg")) |>
  sf::st_transform(epsg)
aop <- sf::st_read(here::here("data/fieldwork/niwot_aop_polygons_2023_12_8_23_analysis_ready_half_diam.gpkg")) |>
  sf::st_transform(epsg)

evt <- terra::rast(here::here('data', 'existing-vegetation-type', 'LF2020_EVT_220_CONUS', 'Tif', 'LC20_EVT_220.tif'))
evtCsv <- read_csv(here::here('data', 'existing-vegetation-type', 'LF2020_EVT_220_CONUS', 'CSV_Data', 'LF20_EVT_220.csv'))
sRockiesAnalysis <- read_csv(here::here("C:/dev/macrosystems-field-planning/data/field_plan_sets/southernRockies_0-001_0-1/southernRockies_0-001_0-1_clean_analysis.csv"))

# EXTRACT
uasVals <- terra::extract(evt, terra::vect(uas), fun = median, method = "simple", bind = TRUE) |>
  sf::st_as_sf() |>
  dplyr::rename(VALUE = EVT_NAME) |>
  dplyr::mutate(VALUE = as.integer(VALUE)) |>
  dplyr::select(imagery, cover_category, cover_subcategory, dead_subcategory, VALUE)
aopVals <- terra::extract(terra::crop(x = evt, y = aop, mask = TRUE, touches = TRUE), terra::vect(aop), fun = median, method = "simple", bind = TRUE) |>
  sf::st_as_sf() |>
  dplyr::rename(VALUE = EVT_NAME) |>
  dplyr::mutate(VALUE = as.integer(VALUE)) |>
  dplyr::select(imagery, cover_category, cover_subcategory, dead_subcategory, VALUE)

allVals <- rbind(uasVals, aopVals) |>
  dplyr::left_join(evtCsv |> dplyr::select(VALUE, EVT_NAME)) |>
  dplyr::filter(!is.na(EVT_NAME))

# SUMMARIZE
summary <- allVals |>
  dplyr::count(cover_category, EVT_NAME) |>
  dplyr::ungroup() |>
  dplyr::filter(EVT_NAME %in% sRockiesAnalysis$EVT_NAME) |>
  sf::st_drop_geometry() |>
  dplyr::arrange(EVT_NAME) |>
  tidyr::pivot_wider(names_from = cover_category, values_from = n)


x <- dplyr::left_join(sRockiesAnalysis, summary)








