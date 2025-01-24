library(here)
library(sf)
library(mapview)
library(tidyverse)

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

#A function to calculate the approximate diameter of the maximum inscribed circle within an irregular polygon. This diameter will be added to the polygons as 'diam'
#I.e. an estimate of the 'minimum diameter' of the irregular polygon
#PARAMETERS
#polys = an sf object representing polygons
#tolerance = Threshold for considering circles to be touching a boundary.
calculate.approx.diameter.maximum.inscribed.circle <- function(polys, tolerance) {
  max.insc.crcs <- geos::geos_maximum_inscribed_crc(polys |>
                                                      geos::as_geos_geometry(), tolerance = tolerance) |>
    sf::st_as_sf() |>
    sf::st_transform(sf::st_crs(polys))
  diam <- c()
  for(i in 1:nrow(max.insc.crcs)) {
    p <- max.insc.crcs[i,]
    d <- p |>
      sf::st_cast('MULTIPOINT') %>%
      sf::st_cast('POINT') %>%
      sf::st_distance(which = "Euclidean") |>
      max()
    diam <- diam |> append(d)
  }
  return(cbind(polys, diam))
}

#A function that uses 'calculate.approx.diameter.maximum.inscribed.circle'
#to buffer a set of polygons inwards such that they are 'half-diameter' polygons
#PARAMETERS
#polys = an sf object representing polygons
#tolerance = Threshold for considering circles to be touching a boundary.
buffer.to.half.diam <- function(polys, tolerance) {
  pWithD <- calculate.approx.diameter.maximum.inscribed.circle(polys, tolerance) |>
    dplyr::filter(is.finite(diam))
  newPolys <- pWithD |>
    dplyr::mutate(geometry = sf::st_buffer(geometry, dist = (diam / 4) * -1)) |>
    dplyr::rename(old_diam = diam)
  return(newPolys)
}

#half_diam_trees <- buffer.set.to.half.diam(trees, tolerance = 0.01) - doesn't seem to work, applying to each individually instead

half_diam_trees <- trees %>%
  split(seq_len(nrow(.))) |>
  purrr::map(buffer.to.half.diam, 0.01) |>
  dplyr::bind_rows()

x <- trees |> dplyr::filter(siteID == "NIWO")
y <- half_diam_trees |> dplyr::filter(siteID == "NIWO")

mapview(x) + mapview(y, col.regions = "red")


# Write out
sf::st_write(trees, here::here('data', 'derived', 'ard_weinstein_trees.gpkg'))
sf::st_write(half_diam_trees, here::here('data', 'derived', 'ard_weinstein_trees_half_diam.gpkg'))


