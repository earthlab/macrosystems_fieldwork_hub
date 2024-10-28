
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


# Select AOP tiles ----

# Get image grids for all sites for selection of AOP tiles
yell_aop_grid <- generate_aop_image_grid(site = "YELL", year = 2023, visit = 5)
#add NIWO & WREF - need to check year & visit for each


