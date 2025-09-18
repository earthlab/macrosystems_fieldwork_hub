

library(sf)
library(here)
library(dplyr)
source(here::here('code', 'functions.R'))


#Derived data directory
dir_derived <- here::here("data/derived")
if (!dir.exists(dir_derived)){
  dir.create(dir_derived)
}

#Derived data directory
dir_manual <- here::here("data/manual")
if (!dir.exists(dir_manual)){
  dir.create(dir_manual)
}

dats <- st_read(here(dir_manual, "uas_polygons_4_22_2025_analysis_ready_meb_manual_fix_2024.geojson"))

dats <- dats |>
  dplyr::mutate(ORIGINAL_OBJECTID = OBJECTID,
                OBJECTID = row_number())


dats_half <- dats %>%
  split(seq_len(nrow(.))) |>
  purrr::map(buffer_to_half_diam, 0.01) |>
  dplyr::bind_rows()

st_write(dats_half, here(dir_derived, "uas_polygons_4_22_2025_analysis_ready_meb_manual_fix_2024_half_diam.geojson"), append = FALSE)

