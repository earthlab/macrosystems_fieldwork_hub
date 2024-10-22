

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
dir_ensure(dir_raw)
dir_ensure(dir_derived)


# Load data
raster <- access_landfire_evt_conus_2023()
raster_cats <- access_landfire_evt_conus_2023_csv()
aoi_polygons <- access_neon_domains_shp()
neon_flightboxes <- access_neon_aop_flight_box_data() #note that flight boxes have the domain data as "D##" instead of just "##"




# Representativeness ----

# Presence of raster value within NEON base plots
sites <- c("YELL", "WREF", "RMNP", "NIWO")

baseplot_freqs <- sites |> purrr::map(~ get_base_plot_freqs(site_id = .x, cat_raster = raster))









# Yellowstone AOI ----
# Create more limited Yellowstone AOI that does not include the bear management zone or the southern part of the flight box that is too far from the road for easy access
yell_flightbox <- neon_flightboxes |> dplyr::filter(siteID == "YELL")  |>
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
aoi <- roads |>
  sf::st_buffer(units::set_units(2, "miles"), nQuadSegs = 100) |> #buffer roads
  sf::st_intersection(yell_flightbox) |> #clip to NEON AOP
  sf::st_difference(bma |>
                      dplyr::filter(NAME == "WASHBURN") |>
                      sf::st_buffer(units::set_units(0.1, "miles"))) # remove closed bma plus a small buffer around closed area


sf::st_write(aoi, here::here(dir_derived, 'yell_aoi.gpkg'), append = FALSE)




# Export for visualization and use ----

# Create a QGIS QML file for visualizing EVT outputs in QGIS. Can be loaded in QGIS an associated with the layer
create.qgis.style.for.paletted.raster.from.csv(styleData = raster_cats %>%
                                                 mutate(across(everything(), ~replace(., is.na(.), "NA"))),
                                               outputQmlPath = here::here('data/derived/landfire_evt_style.qml'),
                                               valueColumn = "VALUE",
                                               labelColumn = "EVT_NAME",
                                               colorScheme = "RGB")





# Select AOP tiles ----

# Get image grids for all sites for selection of AOP tiles
yell_aop_grid <- generate_aop_image_grid(site = "YELL", year = 2023, visit = 5)
#add NIWO & WREF - need to check year & visit for each



