library(terra)
library(sf)
library(here)
library(tidyverse)
library(mapview)

rm(list=ls()) #Ensure empty workspace

evt <- terra::rast("C:/dev/macrosystems-field-planning/data/existing-vegetation-type/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif")
evtCsv <- read_csv("C:/dev/macrosystems-field-planning/data/existing-vegetation-type/LF2020_EVT_220_CONUS/CSV_Data/LF20_EVT_220.csv")


# aopPoly <- sf::st_read(here::here('data', 'derived', 'niwot_aop_polygons_2023_12_8_23_analysis_ready.gpkg')) |>
#   sf::st_transform(terra::crs(evt))


aopPoly <- sf::st_read(here::here('data', 'derived', 'aop_macrosystems_data_07_27_24.gpkg')) |>
  sf::st_transform(terra::crs(evt))


uasPoly <- sf::st_read(here::here('data', 'derived', 'uas_polygons_2023_12_13_23_analysis_ready.gpkg')) |>
  sf::st_transform(terra::crs(evt))


make.evt.join.table <- function(dats) {
  outs <- evt |>
    terra::zonal(terra::vect(dats), fun = "median") %>% 
    cbind(dats, .) |>
    dplyr::mutate(VALUE = as.integer(EVT_NAME)) |>
    dplyr::select(-EVT_NAME) |>
    dplyr::left_join(evtCsv, by = "VALUE") |>
    sf::st_drop_geometry()
    
  return(outs)
}

aopPolyNew <- make.evt.join.table(aopPoly) |>
  dplyr::filter(!is.na(EVT_NAME))
uasPolyNew <- add.evt(uasPoly) |>
  dplyr::filter(!is.na(EVT_NAME))

sf::st_write(aopPolyNew, here::here('data', 'derived', 'aop_polygons_evt_join_table.csv'), append = TRUE)
sf::st_write(uasPolyNew, here::here('data', 'derived', 'uas_polygons_evt_join_table.csv'), append = TRUE)





yell <- aopPolyNew |> dplyr::filter(is.na(location))
yEVT <- yell |> count(EVT_NAME)
yTrees <- yell |> 
  dplyr::count(species)
  

