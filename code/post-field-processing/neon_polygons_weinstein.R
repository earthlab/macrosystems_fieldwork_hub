# This script prepares tree canopy polygons from Ben Weinstein to be used for spectral extraction
# These polygons are those from NEON woody veg surveys that are expected to be visible from above,
# merged with predicted canopy polygons from Weinstein et al 2024.

# Tyler McIntosh 2025


#Setup
rm(list = ls())
library(here)
library(sf)
library(mapview)
library(tidyverse)
source(here::here('code', 'functions.R'))


# Read data
weinstein <- read_csv(here::here('data/manual/weinstein_no_contrib_annotations.csv'))

# Polygons are in UTM for each site, so need to be processed separately
yell <- weinstein |>
  dplyr::filter(siteID == "YELL") |>
  sf::st_as_sf(
    wkt = "geometry",
    crs = 26912
  ) |>
  sf::st_transform(5070)

rmnp <- weinstein |>
  dplyr::filter(siteID == "RMNP") |>
  sf::st_as_sf(
    wkt = "geometry",
    crs = 26913
  ) |>
  sf::st_transform(5070)

niwo <- weinstein |>
  dplyr::filter(siteID == "NIWO") |>
  sf::st_as_sf(
    wkt = "geometry",
    crs = 26913
  ) |>
  sf::st_transform(5070)

wref <- weinstein |>
  dplyr::filter(siteID == "WREF") |>
  sf::st_as_sf(
    wkt = "geometry",
    crs = 26910
  ) |>
  sf::st_transform(5070)

trees <- rbind(yell, rmnp, wref, niwo)

# Merge taxonomic IDs
# Download from https://data.neonscience.org/taxonomic-lists after selecting "Taxon Type - Plant"

taxon <- readr::read_csv(here::here("data", "raw", "OS_TAXON_PLANT-20220330T142149.csv")) |>
  dplyr::select(taxonID, acceptedTaxonID, scientificName, vernacularName)

trees <- trees |>
  dplyr::left_join(taxon, by = join_by(taxonID)) |>
  dplyr::mutate(cover_category = dplyr::case_when(taxonID == "POTR5" ~ "Deciduous",
                                                   taxonID == "POPUL" ~ "Deciduous", 
                                                   taxonID == "ACCI" ~ "Deciduous",
                                                   TRUE ~ "Evergreen"),
                cover_subcategory = NA) |>
  dplyr::rename(species = scientificName)
  
#Generate half-diameter polygons

#half_diam_trees <- buffer.set.to.half.diam(trees, tolerance = 0.01) - doesn't seem to work, applying to each individually instead

half_diam_trees <- trees %>%
  split(seq_len(nrow(.))) |>
  purrr::map(buffer_to_half_diam, 0.01) |>
  dplyr::bind_rows()

x <- trees |> dplyr::filter(siteID == "NIWO")
y <- half_diam_trees |> dplyr::filter(siteID == "NIWO")

mapview(x) + mapview(y, col.regions = "red")


# Write out
sf::st_write(trees,
             here::here('data', 'derived', 'ard_weinstein_trees.gpkg'),
             append = FALSE)
sf::st_write(half_diam_trees,
             here::here('data', 'derived', 'ard_weinstein_trees_half_diam.gpkg'),
             append = FALSE)


