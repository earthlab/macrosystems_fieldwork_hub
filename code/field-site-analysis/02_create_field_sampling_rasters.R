# This script will create rasters of areas not well-represented within NEON AOP
# flight boxes, as defined by landfire EVT classes. These rasters are then meant to be used
# for randomized site selection for the Macrosystems project

#Tyler L. McIntosh

# Setup ----
#rm(list = ls())

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
    "curl",
    "mapview",
    "gdalcubes"),
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

## Load data ----
raster <- access_landfire_evt_conus_2022(access = 'download',
                                         dir_path = dir_raw)
raster_cats <- access_landfire_evt_conus_2022_csv()
region_polygons <- access_data_epa_l3_ecoregions_vsi()
areas_of_interest <- access_neon_aop_flight_box_data() #note that flight boxes have the domain data as "D##" instead of just "##"


## Load data from script 01 ----
# which creates conservative EVT layers
# the data from script 01 needs to be put in a folder in dir_derived called
# "conservative_evt_west_3window" and "conservative_evt_west_5window" respectively
# They are conservative estimates of EVT in that they have had a spatial window filter
# put over them to remove speckle

#3x3 median window
evt3F <- here::here('data', 'derived', 'conservative_evt_west_3window', 'conservative_evt_west_3window.tif')
if(file.exists(evt3F)) {
  evt_conservative_3_window <- terra::rast(evt3F)
} else {
  evt3Fs <- list.files(here::here('data', 'derived', 'conservative_evt_west_3window'), pattern = "\\.tif$", full.names = TRUE)
  merge_list_of_rasters(file_list = evt3Fs,
                        file_final_path = evt3F,
                        datatype = "INT2U",
                        compress = TRUE,
                        write = TRUE)
  evt_conservative_3_window <- terra::rast(evt3F)
}
levels(evt_conservative_3_window) <- terra::cats(raster)[[1]]


#5x5 median window
evt5F <- here::here('data', 'derived', 'conservative_evt_west_5window', 'conservative_evt_west_5window.tif')
if(file.exists(evt5F)) {
  evt_conservative_5_window <- terra::rast(evt5F)
} else {
  evt5Fs <- list.files(here::here('data', 'derived', 'conservative_evt_west_5window'), pattern = "\\.tif$", full.names = TRUE)
  merge_list_of_rasters(file_list = evt5Fs,
                        file_final_path = evt5F,
                        datatype = "INT2U",
                        compress = TRUE,
                        write = TRUE)
  evt_conservative_5_window <- terra::rast(evt5F)
}
levels(evt_conservative_5_window) <- terra::cats(raster)[[1]]



# Export EVT QML file for visualization and use ----

# Create a QGIS QML file for visualizing EVT outputs in QGIS. Can be loaded in QGIS an associated with the layer
create_qgis_style_for_paletted_raster_from_csv(styleData = raster_cats %>%
                                                 mutate(across(everything(), ~replace(., is.na(.), "NA"))),
                                               outputQmlPath = here::here('data/derived/landfire_evt_style.qml'),
                                               valueColumn = "VALUE",
                                               labelColumn = "EVT_NAME",
                                               colorScheme = "RGB")




# Representativeness rasters for field site sampling ----

## Prep polygons ----
### Middle rockies ----
middle_rockies <- region_polygons |>
  dplyr::filter(US_L3NAME == "Middle Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()

middle_rockies_core <- region_polygons |>
  dplyr::filter(US_L3NAME == "Middle Rockies") %>%
  dplyr::mutate(id = seq(1:nrow(.))) |>
  dplyr::filter(id == 12)


#### Establish Yellowstone AOI ----
# Create more limited Yellowstone AOI that does not include the bear management zone or the southern part of the flight box that is too far from the road for easy access
yell_flightbox <- areas_of_interest |> dplyr::filter(siteID == "YELL")  |>
  sf::st_transform(crs = 4326)


# NOTE this yellowstone bear management dataset is available here: https://nam10.safelinks.protection.outlook.com/?url=https%3A%2F%2Fnps.maps.arcgis.com%2Fhome%2Fitem.html%3Fid%3Dc9e7cdf8ce6e48c2ad5f65b1e9e92548%23overview&data=05%7C02%7CTyler.L.McIntosh%40colorado.edu%7Cff8749ebed8d4e20667c08dce7dad938%7C3ded8b1b070d462982e4c0b019f46057%7C1%7C0%7C638640174358195095%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C0%7C%7C%7C&sdata=Ip5m1jAkS7nm8BTyOK8QoIBrQ1cBp%2FwFq8BpO1Ntp%2BU%3D&reserved=0
# This code reads it from the REST service
bma <- access_ynp_bear_management_areas() |>
  sf::st_transform(crs = 4326)

# Get road data
roads_data <- osmdata::opq(bbox = sf::st_bbox(yell_flightbox)) |>
  osmdata::add_osm_feature(key = "highway",
                           key_exact = FALSE,
                           value_exact = FALSE,
                           match_case = FALSE) |>
  osmdata::osmdata_sf()
roads <- roads_data$osm_lines |>
  dplyr::filter(highway != "path") |>
  dplyr::mutate(group = 1) |>
  group_by(group) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()

#create aoi
yell_aoi <- roads |>
  sf::st_buffer(units::set_units(2, "miles"), nQuadSegs = 100) |> #buffer roads
  sf::st_intersection(yell_flightbox) |> #clip to NEON AOP
  sf::st_difference(bma |>
                      dplyr::filter(NAME == "WASHBURN") |>
                      sf::st_buffer(units::set_units(0.1, "miles"))) # remove closed bma plus a small buffer around closed area


sf::st_write(yell_aoi, here::here(dir_derived, 'yell_aoi.gpkg'), append = FALSE)


### Southern rockies ----
southern_rockies <- region_polygons |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()

niwo <- areas_of_interest |> dplyr::filter(siteID == "NIWO")


### Cascades ----
cascades <- region_polygons |>
  dplyr::filter(US_L3NAME == "Cascades") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()

cascades_core <- region_polygons |>
  dplyr::filter(US_L3NAME == "Cascades") %>%
  dplyr::mutate(id = seq(1:nrow(.))) |>
  dplyr::filter(id == 4)

cascades_core_buff <- cascades_core |>
  sf::st_buffer(dist = 1000, nQuadSegs = 1000)

wref <- areas_of_interest |> dplyr::filter(siteID == "WREF")

## Create rasters ----

tic()
results <- representative_categorical_cover_analysis(raster = evt_conservative_3_window,
                                                     raster_cat_df = raster_cats,
                                                     region_shape = southern_rockies,
                                                     aoi_shape = niwo,
                                                     run_name = "southern_rockies_evt3",
                                                     cat_base_column_name = "VALUE",
                                                     region_drop_perc = 0.001,
                                                     aoi_drop_perc = 0.001,
                                                     drop_classes = NA,
                                                     drop_classes_column_name = NA,
                                                     out_rast_values = "RAW",
                                                     out_rast_type = "NOT_REP",
                                                     out_dir = dir_field_rasters,
                                                     new_sub_dir = FALSE)
toc()

tic()
results <- representative_categorical_cover_analysis(raster = evt_conservative_3_window,
                                                     raster_cat_df = raster_cats,
                                                     region_shape = middle_rockies_core,
                                                     aoi_shape = yell_aoi,
                                                     run_name = "middle_rockies_core_evt3",
                                                     cat_base_column_name = "VALUE",
                                                     region_drop_perc = 0.001,
                                                     aoi_drop_perc = 0.001,
                                                     drop_classes = NA,
                                                     drop_classes_column_name = NA,
                                                     out_rast_values = "RAW",
                                                     out_rast_type = "NOT_REP",
                                                     out_dir = dir_field_rasters,
                                                     new_sub_dir = FALSE)
toc()

tic()
results <- representative_categorical_cover_analysis(raster = evt_conservative_3_window,
                                                     raster_cat_df = raster_cats,
                                                     region_shape = cascades_core_buff,
                                                     aoi_shape = wref,
                                                     run_name = "cascades_core_buff_evt3",
                                                     cat_base_column_name = "VALUE",
                                                     region_drop_perc = 0.001,
                                                     aoi_drop_perc = 0.001,
                                                     drop_classes = NA,
                                                     drop_classes_column_name = NA,
                                                     out_rast_values = "RAW",
                                                     out_rast_type = "NOT_REP",
                                                     out_dir = dir_field_rasters,
                                                     new_sub_dir = FALSE)
toc()


