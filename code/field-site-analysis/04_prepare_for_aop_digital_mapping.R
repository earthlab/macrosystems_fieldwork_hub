# This script prepares for digital mapping within the
# NEON AOP flight box areas by:
# 1) Creating 'image grid' polygons for sites to help identify the RGB tiles to download and process
# 2) Establishing a randomized set of points to consider visiting at AOP sites


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
dir_sampling <- here::here("data/derived/sampling_options")
dir_ensure(dir_raw)
dir_ensure(dir_derived)
dir_ensure(dir_field_rasters)
dir_ensure(dir_sampling)

# Load data

evt_cats <- access_landfire_evt_conus_2022_csv()
aop_flight_boxes <- access_neon_aop_flight_box_data() #note that flight boxes have the domain data as "D##" instead of just "##"

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
levels(evt_conservative_5_window) <- evt_cats |> dplyr::mutate(VALUE = as.integer(VALUE))


# Create AOP tile polygons ----

# Get image grids for all sites for selection of AOP tiles
yell_aop_grid <- generate_aop_image_grid(site = "YELL", year = 2023, visit = 5)
#add NIWO & WREF - need to check year & visit for each




# Create AOP potential visit locations ----




## WREF ----
wref <- aop_flight_boxes |>
  dplyr::filter(siteID == "WREF")


generate_aop_plots(
  aop = wref,
  lc = evt_conservative_5_window,
  numPlotsEach = 5,
  name = "WREF",
  lcName = "evt5window_new1000",
  outDir = dir_sampling,
  sma = "USFS",
  distFromRoadsMax = 1000
)


## YELL ----

yell <- aop_flight_boxes |>
  dplyr::filter(siteID == "YELL")

generate_aop_plots(
  aop = yell,
  lc = evt_conservative_5_window,
  numPlotsEach = 3,
  name = "YELL",
  lcName = "evt5window_new1000",
  outDir = dir_sampling,
  sma = "NPS",
  distFromRoadsMax <- 3000
)


