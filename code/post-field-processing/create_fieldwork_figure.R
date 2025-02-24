# This script creates figures and tables describing the Macrosystems project data
# It will AUTOMATICALLY install and load any missing packages
# Tyler L. McIntosh
# 2/13/25

rm(list = ls())

# Load necessary libraries
library(here)

source(here::here('code/functions.R'))

install_and_load_packages(c("sf",
                            "tidyverse",
                            "elevatr",
                            "terra",
                            "exactextractr",
                            "purrr",
                            "kableExtra",
                            "rlang",
                            "tigris",
                            "webshot2"))

# Set EPSG
epsg <- 5070

# Function to load and transform an sf object
load_transform_sf <- function(file_path, dataset_name, epsg) {
  sf_object <- st_read(here(file_path)) %>%
    st_transform(crs = epsg) %>%
    mutate(source_dataset = dataset_name) # Add dataset origin
  return(sf_object)
}

# Load, transform, prepare datasets ----
drone <- load_transform_sf("data/derived/uas_polygons_2_14_2025_analysis_ready.geojson", "uas", epsg) |>
  mutate(extraction_group = plotID_clean) |>
  select(cover_category, extraction_group, ecoregion, source_dataset)
aop_field <- load_transform_sf("data/derived/aop_polygons_2_14_2025_analysis_ready.geojson", "aop_field", epsg)  |>
  mutate(extraction_group = aop_site) |>
  select(cover_category, extraction_group, ecoregion, source_dataset)
aop_trees <- load_transform_sf("data/derived/ard_weinstein_trees.geojson", "aop_trees", epsg) |>
  mutate(extraction_group = siteID,
         ecoregion = case_when(siteID == "YELL" ~ "MiddleRockies",
                               siteID == "RMNP" ~ "SouthernRockies",
                               siteID == "NIWO" ~ "SouthernRockies",
                               siteID == "WREF" ~ "Cascades",
                               TRUE ~ NA)) |>
  select(cover_category, extraction_group, ecoregion, source_dataset)
plots <- load_transform_sf("data/manual/macrosystems_plots_23_24.geojson", "uas_plots", epsg)

# Merge & get area
all <- rbind(drone,
             aop_trees,
             aop_field) |>
  st_area_to_poly(nm = area_m2)


# Access Landfire EVT data, ecoregions, AOP

evt <- access_landfire_evt_conus_2022(access = "download",
                                      dir_path = here::here("data/raw"))
evt_csv <- access_landfire_evt_conus_2022_csv()

epa_l3 <- access_data_epa_l3_ecoregions_vsi() |>
  sf::st_transform(epsg)

neon_aop <- access_neon_aop_flight_box_data() |>
  sf::st_transform(epsg)


# Add data on other variables ----
# Specialized function to add both EVT and topo data to the polygons
extract_vars <- function(sf_subset) {
  sf_subset <- extract_topo(sf_subset,
                            centroid = FALSE,
                            fun = "mean")
  
  lf_interest <- terra::crop(evt, sf_subset |> sf::st_bbox())
  
  # Compute centroids and extract values
  centroids <- sf::st_centroid(sf_subset)
  extracted_vals <- terra::extract(evt, centroids, ID = FALSE)
  
  # Add extracted values to sf object
  sf_subset_plus <- sf_subset |>
    dplyr::mutate(evt = extracted_vals[,1]) |>
    dplyr::left_join(evt_csv, by = dplyr::join_by(evt == EVT_NAME))
  
  return(sf_subset_plus)
}

# Apply extract_vars to each extraction group (in parallel) and merge results
all_dats_added <- all |>
  dplyr::group_split(extraction_group) |>   # Split by site
  furrr::future_map_dfr(extract_vars) |>   # Apply function and merge results
  dplyr::mutate(all = "all_data")

# Calculate summary statistics & graphics ----

## Topo ----

create_density_plots <- function(x, nm) {
  gg <- ggplot2::ggplot(data = all_dats_added) +
    geom_density(aes(x = {{x}}, colour = source_dataset)) +
    geom_density(aes(x = {{x}}, color = all))
  gg_facet <- gg +
    facet_wrap(~ ecoregion)
  
  ggsave(filename = here("figs", paste0("field_data_summary_", nm, ".jpg")),
         plot = gg)
  ggsave(filename = here("figs", paste0("field_data_summary_facet_", nm, ".jpg")),
         plot = gg_facet)
}

create_density_plots(slope, "slope")
create_density_plots(aspect, "aspect")
create_density_plots(elevation, "elevation")


## PFT & EVT ----

create_bar_charts <- function(y, nm) {
  gg <- ggplot(data = all_dats_added) +
    geom_bar(aes(y = {{y}}, fill = source_dataset))
  gg_facet <- gg +
    facet_wrap(~ ecoregion)
  
  ggsave(filename = here("figs", paste0("field_data_summary_", nm, ".jpg")),
         plot = gg)
  ggsave(filename = here("figs", paste0("field_data_summary_facet_", nm, ".jpg")),
         plot = gg_facet)
}

create_bar_charts(cover_category, "cover_category")
create_bar_charts(EVT_GP_N, "EVT_GP_N")



# Create count summaries

create_count_summary_table <- function(cols, rows) {
  
  c_nm <- rlang::as_name(enquo(cols))
  r_nm <- rlang::as_name(enquo(rows))
  
  t <- all_dats_added |>
    sf::st_drop_geometry() |>
    group_by({{cols}}, {{rows}}) |>
    summarize(observations = n()) |>
    ungroup() |>
    pivot_wider(names_from = {{cols}},
                values_from = observations) %>%
    mutate(all = rowSums(across(2:length(names(.))))) %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
  
  kableExtra::kbl(t,
                  caption = paste0("Observation counts by ", c_nm, " X ", r_nm)) |>
    kableExtra::kable_classic(, full_width = F) |>
    kableExtra::save_kable(file = here("figs", paste0("n_field_data_summary_", c_nm, "_X_", r_nm, ".html")))
}

create_count_summary_table(cols = source_dataset,
                                rows = ecoregion)

create_count_summary_table(cols = cover_category,
                           rows = ecoregion)

create_count_summary_table(cols = ecoregion,
                           rows = EVT_GP_N)

## Area ----
#Create area subset summaries

all_area_dataset_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(source_dataset, ecoregion) |>
  summarize(mean_area = mean(area_m2)) |>
  ungroup() |>
  pivot_wider(names_from = ecoregion,
              values_from = mean_area) |>
  mutate(subset_type = "Dataset") |>
  rename(subset = source_dataset)

all_area_pft_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(cover_category, ecoregion) |>
  summarize(mean_area = mean(area_m2)) |>
  ungroup() |>
  pivot_wider(names_from = ecoregion,
              values_from = mean_area) |>
  mutate(subset_type = "PFT") |>
  rename(subset = cover_category)

all_area_eco_summary <- rbind(all_area_dataset_eco,
                              all_area_pft_eco) |>
  select(subset_type, subset, Cascades, MiddleRockies, SouthernRockies)





# Create large area summary table
all_area_pft <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(cover_category) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "PFT") |>
  rename(subset = cover_category)

all_area_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(ecoregion) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "Ecoregion") |>
  rename(subset = ecoregion)

all_area_dataset <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(source_dataset) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "Dataset") |>
  rename(subset = source_dataset)


all_area <- all_dats_added |>
  sf::st_drop_geometry() |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "All",
         subset = "All")

all_area_summary <- rbind(all_area_pft,
                          all_area_eco,
                          all_area_dataset,
                          all_area) |>
  select(subset_type, subset, mean_area, median_area)


# Export kables
kableExtra::kbl(all_area_summary,
                col.names = c("Subset Type",
                              "Subset",
                              "Mean area (m^2)",
                              "Median area (m^2)"),
                caption = "Polygon area data by subsets") |>
  kable_classic(, full_width = F) |>
  save_kable(file = here("figs/area_summary.html"))

kableExtra::kbl(all_area_eco_summary,
                col.names = c("Subset Type",
                              "Subset",
                              "Cascades",
                              "Middle Rockies",
                              "Southern Rockies"),
                caption = "Polygon mean area in m^2 data by ecoregion subsets") |>
  kable_classic(, full_width = F) |>
  save_kable(file = here("figs/area_ecoregion_summary.html"))



## Geographic ----

# access geographic data and export to use in QGIS for carto

usa <- tigris::nation() |>
  sf::st_transform(epsg)

western_states <- tigris::states() |>
  sf::st_transform(epsg) |>
  dplyr::filter(STUSPS %in% c("CA", "NV", "WA", "OR", "ID", "MT", "WY", "CO", "UT", "AZ", "NM"))

states <- tigris::states() |>
  sf::st_transform(epsg)


epa_l3 <- access_data_epa_l3_ecoregions_vsi() |>
  sf::st_transform(epsg)

s_rockies <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Southern Rockies")
m_rockies <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Middle Rockies")
cascades <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Cascades")

neon_sites_interest <- neon_aop |>
  dplyr::filter(siteID == "YELL" |
                  siteID == "RMNP" |
                  siteID == "NIWO" |
                  siteID == "WREF")

neon_sites_interest_points <- neon_sites_interest |>
  dplyr::group_by(siteID) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  sf::st_centroid()

# yell <- neon_aop |>
#   dplyr::filter(siteID == "YELL")
# rmnp <- neon_aop |>
#   dplyr::filter(siteID == "RMNP")
# niwo <- neon_aop |>
#   dplyr::filter(siteID == "NIWO")
# wref <- neon_aop |>
#   dplyr::filter(siteID == "WREF")

uas_plots <- sf::st_read(here::here("data", "manual", "macrosystems_plots_23_24.geojson")) |>
  sf::st_transform(epsg) |>
  sf::st_centroid() %>%
  dplyr::mutate(fid = seq(1:nrow(.)))

all_poly_points <- all |>
  sf::st_centroid()

dir_field_fig_data <- here::here('data', 'raw', 'field_fig')
dir_ensure(dir_field_fig_data)
sf::st_write(usa, here::here(dir_field_fig_data, "usa.gpkg"), append = FALSE)
sf::st_write(western_states, here::here(dir_field_fig_data, "west_states.gpkg"), append = FALSE)
sf::st_write(states, here::here(dir_field_fig_data, "states.gpkg"), append = FALSE)
sf::st_write(m_rockies, here::here(dir_field_fig_data, "m_rockies.gpkg"), append = FALSE)
sf::st_write(s_rockies, here::here(dir_field_fig_data, "s_rockies.gpkg"), append = FALSE)
sf::st_write(cascades, here::here(dir_field_fig_data, "cascades.gpkg"), append = FALSE)
sf::st_write(yell, here::here(dir_field_fig_data, "yell.gpkg"), append = FALSE)
sf::st_write(rmnp, here::here(dir_field_fig_data, "rmnp.gpkg"), append = FALSE)
sf::st_write(niwo, here::here(dir_field_fig_data, "niwo.gpkg"), append = FALSE)
sf::st_write(wref, here::here(dir_field_fig_data, "wref.gpkg"), append = FALSE)
sf::st_write(neon_sites_interest, here::here(dir_field_fig_data, "neon_sites_interest.gpkg"), append = FALSE)
sf::st_write(neon_sites_interest_points, here::here(dir_field_fig_data, "neon_sites_interest_points.gpkg"), append = FALSE)
sf::st_write(uas_plots, here::here(dir_field_fig_data, "uas_points.gpkg"), append = FALSE)
sf::st_write(all_poly_points, here::here(dir_field_fig_data, "field_poly_points.gpkg"), append = FALSE)





# ggplot2::ggplot() +
#   geom_sf(data = usa, aes(geometry = geometry)) +
#   geom_sf(data = neon_aop, aes(geometry = geometry))
# 
# 
# 
# ggplot2::ggplot() +
#   geom_sf(data = epa_l3 |> filter(NA_L3NAME == "Southern Rockies")) +
#   #geom_sf(data = usa, aes(geometry = geometry)) +
#   geom_sf(data = neon_aop, aes(geometry = geometry))




