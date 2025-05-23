library(here)
library(sf)

epsg <- 5070

# Function to load and transform an sf object
load_transform_sf <- function(file_path, dataset_name, epsg) {
  sf_object <- st_read(here(file_path)) %>%
    st_transform(crs = epsg) %>%
    mutate(source_dataset = dataset_name) # Add dataset origin
  return(sf_object)
}



# Load, transform, prepare datasets ----
aop_field <- load_transform_sf("data/derived/aop_polygons_4_22_2025_analysis_ready.geojson", "aop_field", epsg)  |>
  mutate(siteID = aop_site) |>
  select(cover_category, cover_subcategory, siteID, ecoregion, source_dataset, imagery_year)
aop_trees <- load_transform_sf("data/derived/ard_weinstein_trees_most_recent_survey.geojson", "aop_trees", epsg) |>
  mutate(
         ecoregion = case_when(siteID == "YELL" ~ "MiddleRockies",
                               siteID == "RMNP" ~ "SouthernRockies",
                               siteID == "NIWO" ~ "SouthernRockies",
                               siteID == "WREF" ~ "Cascades",
                               TRUE ~ NA)) |>
  select(cover_category, cover_subcategory, siteID, ecoregion, source_dataset, tile_year) |>
  rename(imagery_year = tile_year)

# Merge & get area
aop_all <- rbind(
             aop_trees,
             aop_field) |>
  st_area_to_poly(nm = area_m2)



sf::st_write(aop_all, here::here('data/derived/ard_aop_combined_for_classifier.geojson'), append = FALSE)



# Load, transform, prepare datasets ----
aop_field_hd <- load_transform_sf("data/derived/aop_polygons_4_22_2025_analysis_ready_half_diam.geojson", "aop_field", epsg)  |>
  mutate(siteID = aop_site) |>
  select(cover_category, cover_subcategory, siteID, ecoregion, source_dataset, imagery_year)
aop_trees_hd <- load_transform_sf("data/derived/ard_weinstein_trees_most_recent_survey_half_diam.geojson", "aop_trees", epsg) |>
  mutate(
         ecoregion = case_when(siteID == "YELL" ~ "MiddleRockies",
                               siteID == "RMNP" ~ "SouthernRockies",
                               siteID == "NIWO" ~ "SouthernRockies",
                               siteID == "WREF" ~ "Cascades",
                               TRUE ~ NA)) |>
  select(cover_category, cover_subcategory, siteID, ecoregion, source_dataset, tile_year) |>
  rename(imagery_year = tile_year)

# Merge & get area
aop_all_hd <- rbind(
             aop_trees_hd,
             aop_field_hd) |>
  st_area_to_poly(nm = area_m2)


sf::st_write(aop_all, here::here('data/derived/ard_aop_combined_for_classifier_half_diam.geojson'), append = FALSE)





