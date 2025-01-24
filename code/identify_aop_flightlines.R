# This script identifies and prioritizes AOP flightlines for processing
#Tyler L McIntosh 2025

# Input overview
# aop_macrosystems_data_1_7_25.geojson is field-collected polygons stored in ArcGIS online
# aop_uas_underflights_2023.geojson is a manually created file showing the areas flown by UAS during the AOP overflight in 2023
# NEON_refl-surf-bidir-flightline-2023 is an unzipped directory containing KML files representing the AOP flight lines of interest
# These flight lines can be downloaded from the NEON Data Portal.
# Note that the KML data from 2023 is for the bidirectional (i.e. corrected) flightlines, since NEON no longer posts KML flight lines with the directional data.
# NEON has confirmed that the KMLs would be the same between datasets


library(sf)
library(here)
library(purrr)
library(tidyverse)
library(mapview)

rm(list = ls())

# Functions ----
# Function to fix file naming convention issue in 2023 provisional data
# Name in attribute table of kmls is old naming convention, name of files is new naming convention
fix_kml_2023 <- function(f) {
  poly <- sf::st_read(f)
  nm <- basename(f) %>%
    gsub("_DPQA_", "_DP1_", .) %>%
    gsub("_boundary.kml", "_directional_reflectance", .)
  poly <- poly |>
    dplyr::mutate(flightline = nm)
  return(poly)
}

# Function to fix file naming in 2020 data
# Name in attribute table of kmls is incomplete
fix_kml_2020 <- function(f) {
  poly <- sf::st_read(f)
  nm <- basename(f) %>%
    gsub("_hsi_kml_0000.kml", "_reflectance", .)
  nm <- paste0("NEON_D13_NIWO_DP1_", nm)
  poly <- poly |>
    dplyr::mutate(flightline = nm)
  return(poly)
}

fix_invalid_flightlines <- function(fl) {
  # Check for invalid geometries
  if (any(!sf::st_is_valid(fl))) {
    print("Some geometries in flight_lines_polygons are still invalid, fixing.")
    
    invalid_flight_lines <- fl[!sf::st_is_valid(fl), ]
    valid_flight_lines <- fl[sf::st_is_valid(fl), ]
    
    #Easy fixes aren't working, need to turn off geos engine
    sf::sf_use_s2(FALSE) 
    fixed_flight_lines <- sf::st_cast(invalid_flight_lines, "MULTIPOLYGON", warn = FALSE) |>
      sf::st_buffer(dist = 0) |>
      sf::st_make_valid()
    sf::sf_use_s2(TRUE)
    
    #re-merge data
    fl_fixed <- rbind(valid_flight_lines, fixed_flight_lines)
    
    return(fl_fixed)
  } else {
    return(fl)
  }
}


# Load field data ----
field_polys <- sf::st_read(here::here("data", "raw", "aop_macrosystems_data_1_7_25.geojson")) |>
  sf::st_make_valid()

crs <- sf::st_crs(field_polys)

underflight_polys <- sf::st_read(here::here("data", "manual", "aop_uas_underflights_2023.geojson")) |>
  sf::st_make_valid() |>
  sf::st_transform(crs)

# Load and manage flightline data ----
## Load and manage 2023 flightlines ----
flight_line_files_2023 <- list.files(path = here::here("data", "raw", "NEON_refl-surf-bidir-flightline-2023"),
                           pattern = ".kml$",
                           recursive = TRUE,
                           full.names = TRUE)

flight_lines_2023 <- flight_line_files_2023 |>
  purrr::map(fix_kml_2023) %>%
  do.call(rbind, .) |>
  sf::st_transform(crs) |>
  sf::st_make_valid()

flight_lines_polygons_2023 <- flight_lines_2023 %>%
  dplyr::filter(sf::st_geometry_type(.) != "POINT") |>
  sf::st_make_valid() |>
  fix_invalid_flightlines() |>
  dplyr::mutate(year = 2023)


## Load and manage 2020 flightlines for NIWO ----
flight_line_files_2020 <- list.files(path = here::here("data", "raw", "NEON_refl-surf-dir-ortho-line-2020"),
                                     pattern = ".kml$",
                                     recursive = TRUE,
                                     full.names = TRUE)

flight_lines_2020 <- flight_line_files_2020 |>
  purrr::map(fix_kml_2020) %>%
  do.call(rbind, .) |>
  sf::st_transform(crs) |>
  sf::st_make_valid()

flight_lines_polygons_2020 <- flight_lines_2020 %>%
  dplyr::filter(sf::st_geometry_type(.) != "POINT") |>
  sf::st_make_valid() |>
  fix_invalid_flightlines() |>
  dplyr::mutate(year = 2020)

# Merge all flightlines and add metadata
all_flight_line_polygons <- rbind(flight_lines_polygons_2020,
                                  flight_lines_polygons_2023) |>
  dplyr::mutate(site_code = case_when(
    grepl("YELL", flightline) ~ "YELL",
    grepl("WREF", flightline) ~ "WREF",
    grepl("RMNP", flightline) ~ "RMNP",
    grepl("NIWO", flightline) ~ "NIWO",
    TRUE ~ NA_character_  # Default to NA if no match
  )) |>
  dplyr::mutate(product_code = "DP1.30006.001") |>
  dplyr::mutate(year_month = str_extract(flightline, "\\d{8}") |>
                  str_replace("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2")) |>
  dplyr::select(-Description, -Name) |>
  dplyr::arrange(site_code)

# Add priority stamps ----

## AOP field mapping polygons lines ----
field_flight_line_polygons <- all_flight_line_polygons |>
  dplyr::filter(site_code == "YELL" |
                  site_code == "WREF" |
                  (site_code == "NIWO" & year == 2020)) |>
  sf::st_filter(field_polys) |>
  dplyr::mutate(priority = "1.HIGH - field polygon flightline") |>
  sf::st_drop_geometry()

## Underflight lines ----
underflight_flight_line_polygons <- all_flight_line_polygons |>
  dplyr::filter(site_code == "NIWO" & year == 2023) |>
  sf::st_filter(underflight_polys) |>
  dplyr::mutate(priority = "2.MODERATE - UAS underflight flightline") |>
  sf::st_drop_geometry()

## All other lines ----
classification_flight_line_polygons <- all_flight_line_polygons |>
  dplyr::filter(site_code == "YELL" |
                  site_code == "WREF" |
                  site_code == "RMNP" |
                  (site_code == "NIWO" & year == 2020)) |>
  dplyr::mutate(priority = "3.LOW - classification flightline") |>
  sf::st_drop_geometry() |>
  dplyr::anti_join(field_flight_line_polygons, by = "flightline")



# Merge and export ----

flightlines_for_processing <- rbind(field_flight_line_polygons,
                                    underflight_flight_line_polygons,
                                    classification_flight_line_polygons) |>
  dplyr::arrange(priority, site_code) |>
  dplyr::select(priority, flightline, site_code, year_month, product_code)

write.csv(flightlines_for_processing,
          file = here::here("data", "derived", "aop_flightlines_for_processing.csv"))



