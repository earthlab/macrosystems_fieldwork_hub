

# Setup ----
rm(list = ls())

options(scipen = 999)

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
    "gdalcubes",
    "tictoc",
    "tigris"),
  auto_install = "y"
)

# Set up necessary data directories
dir_ensure(here::here("data"))
dir_raw <- here::here("data/raw")
dir_derived <- here::here("data/derived")
dir_ensure(dir_raw)
dir_ensure(dir_derived)


#Stream data sources
# tic()
raster <- access_landfire_evt_conus_2022(access = 'download',
                                         dir_path = dir_raw)
# # 2023 - on-campus run time: 250 seconds; cyverse R run time campus: 164 seconds 
# # 2022 - cyverse R run time campus: 500 seconds; on-campus run time: 667 seconds
# toc()
#raster <- terra::rast(here::here(dir_raw, "LF2022_EVT_230_CONUS/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif"))
activeCat(raster) <- 0

raster_cats <- access_landfire_evt_conus_2022_csv() 
region_polygons <- access_neon_domains_shp()
#region_polygons <- access_data_epa_l3_ecoregions_vsi()
areas_of_interest <- access_neon_aop_flight_box_data() #note that flight boxes have the domain data as "D##" instead of just "##"


areas_of_interest <- areas_of_interest |>
  sf::st_transform(sf::st_crs(region_polygons))


# Operate ----

#Prep region & AOI sets

# Get CONUS bounds to spatial subset
conus <- tigris::states() |>
  dplyr::filter(!STUSPS %in% c("AK", "HI", "PR", "VI", "MP", "GU", "AS")) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  sf::st_transform(sf::st_crs(region_polygons))

# Merge regions and aois
region_polygons_merged <- region_polygons |>
  sf::st_cast("POLYGON") %>% #split out multipolygons
  dplyr::filter(sf::st_intersects(., conus, sparse = FALSE)) |>
  dplyr::group_by(DomainID, DomainName) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  dplyr::ungroup() |>
  dplyr::arrange(DomainID)

areas_of_interest_merged <- areas_of_interest |>
  sf::st_make_valid() %>% #force validity, duplicate vertex error
  dplyr::filter(sf::st_intersects(., conus, sparse = FALSE)) |>
  dplyr::group_by(domain, domainName) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  dplyr::ungroup() |>
  dplyr::rename(DomainName = domainName) |>
  dplyr::mutate(DomainID = as.integer(substr_right(domain, 2))) |>
  dplyr::arrange(DomainID)

#Ensure that the same sets are in both
common_domain_ids <- lubridate::intersect(region_polygons_merged$DomainID, areas_of_interest_merged$DomainID)
region_polygons_merged <- region_polygons_merged %>%
  dplyr::filter(DomainID %in% common_domain_ids)
areas_of_interest_merged <- areas_of_interest_merged %>%
  dplyr::filter(DomainID %in% common_domain_ids)

#For outputs
dir_representative <- here::here(dir_derived, "representative_analyses_lens")
dir_ensure(dir_representative)


# tic()
# results <- representative_categorical_cover_analysis(raster = raster,
#                                                      raster_cat_df = raster_cats,
#                                                      region_shape = region_polygons_merged[12,],
#                                                      aoi_shape = areas_of_interest_merged[12,],
#                                                      region_name = paste0("neon_domains_","TEST"),
#                                                      cat_base_column_name = "VALUE",
#                                                      region_drop_perc = 0,
#                                                      aoi_drop_perc = 0.001,
#                                                      drop_classes = NA,
#                                                      drop_classes_column_name = NA,
#                                                      out_rast_values = "BOTH",
#                                                      out_dir = dir_representative)
# toc()
# #1020-50 seconds to run locally with tif downloaded

tic()
all_region_results <- purrr::pmap(list(region_shape = split(region_polygons_merged, seq(nrow(region_polygons_merged))),
                                       aoi_shape = split(areas_of_interest_merged, seq(nrow(areas_of_interest_merged))),
                                       region_name = paste0("neon_domains_",region_polygons_merged$DomainName)),
                                  representative_categorical_cover_analysis,
                                  raster = raster,
                                  raster_cat_df = raster_cats,
                                  cat_base_column_name = "VALUE",
                                  region_drop_perc = 0,
                                  aoi_drop_perc = 0.001,
                                  drop_classes = NA,
                                  drop_classes_column_name = NA,
                                  out_rast_values = "PERC_COVER",
                                  out_dir = dir_representative)
toc() #just under 4 hours

# Create a dataframe with all percentages and export
result_df <- purrr::map_dfr(all_region_results, ~ tibble(
  region_name = .x$analysis_name,
  perc_area_not_represented = .x$perc_area_not_represented
))
readr::write_csv(result_df, here::here(dir_representative, "neon_domains_results.csv"))

# Create a national single-layer perc cover not-represented raster
not_rep_tif_files <- list.files(path = dir_representative,
                                 pattern = "not_rep_perc_cover\\.tif$",
                                 full.names = TRUE,
                                 recursive = TRUE)
national_not_rep <- not_rep_tif_files %>%
  purrr::map(.x = ., terra::rast) |>
  terra::sprc() |>
  terra::mosaic(fun = min)
terra::writeRaster(national_not_rep, here::here(dir_representative, "national_not_rep_perc_cover.tif"))
  



# # TESTING
# 
# 
# #Southern rockies
# region_shape <- region_polygons |>
#   dplyr::filter(US_L3NAME == "Southern Rockies") |>
#   dplyr::group_by(US_L3NAME) |>
#   dplyr::summarise(geometry = st_union(geometry)) |>
#   dplyr::ungroup()
# 
# aoi_shape <- areas_of_interest |>
#   dplyr::filter(siteID == "NIWO")
# 
# tshape <- areas_of_interest |>
#   dplyr::filter(siteID == "NIWO") |>
#   sf::st_transform(terra::crs(raster))
# 
# t <- raster |> terra::crop(tshape)
# 
# 
#   
# tic()
# results <- representative_categorical_cover_analysis(raster = raster,
#                                                      raster_cat_df = raster_cats,
#                                                      region_shape = region_shape,
#                                                      aoi_shape = aoi_shape,
#                                                      region_name = "TEST",
#                                                      cat_base_column_name = "VALUE",
#                                                      region_drop_perc = 0,
#                                                      aoi_drop_perc = 0.001,
#                                                      drop_classes = NA,
#                                                      drop_classes_column_name = NA,
#                                                      return_rast_values_as_landscape_perc = TRUE)
# toc() #on-campus run time:
# 
# 
# terra::writeRaster(results$raster_not_represented, here::here(dir_derived, "test_sr_notrep_perc.tif"))
# 
# 
# 
# 
# # Crop sub-regions for analysis
# print(paste0('Cropping to region', region_name))
# tic()
# larger_region_raster <- crop_careful_universal(raster = raster, vector = region_shape, mask = TRUE, verbose = TRUE) 
# toc()
# tic()
# aoi_raster <- crop_careful_universal(raster = larger_region_raster, vector = aoi_shape, mask = TRUE, verbose = TRUE)
# toc()
# 
# # Analyze
# landcover_analysis_output <- analyze_categorical_cover(aoi_raster = aoi_raster,
#                                                larger_region_raster = larger_region_raster,
#                                                raster_cat_df = raster_cats,
#                                                cat_base_column = "VALUE")
# 
# landcover_analysis_output2 <- analyze_categorical_cover(aoi_raster = aoi_raster,
#                                                        larger_region_raster = larger_region_raster,
#                                                        raster_cat_df = raster_cats,
#                                                        cat_base_column = "VALUE",
#                                                        group = TRUE,
#                                                        cat_group_column = "EVT_GP_N")
# 
# 
# 
# 
# tic()
# test <- representative_categorical_cover_analysis(raster = raster,
#                                                   raster_cat_df = raster_cats,
#                                                   region_shape = region_shape,
#                                                   aoi_shape = aoi_shape,
#                                                   cat_base_column_name = "VALUE",
#                                                   group = TRUE,
#                                                   cat_group_column_name = "EVT_GP_N",
#                                                   region_drop_perc = NA,
#                                                   drop_classes = NA,
#                                                   drop_classes_column_name = NA)
# toc()
# 
# 
# 
# 
# tic()
# test2 <- representative_categorical_cover_analysis(raster = raster,
#                                                   raster_cat_df = raster_cats,
#                                                   region_shape = region_shape,
#                                                   aoi_shape = aoi_shape,
#                                                   cat_base_column_name = "VALUE",
#                                                   group = TRUE,
#                                                   cat_group_column_name = "EVT_GP_N",
#                                                   region_drop_perc = 0.001,
#                                                   drop_classes = NA,
#                                                   drop_classes_column_name = NA)
# toc()
# 
# 
# 
# 
# tic()
# test3 <- representative_categorical_cover_analysis(raster = raster,
#                                                    raster_cat_df = raster_cats,
#                                                    region_shape = region_shape,
#                                                    aoi_shape = aoi_shape,
#                                                    cat_base_column_name = "VALUE",
#                                                    group = TRUE,
#                                                    cat_group_column_name = "EVT_GP_N",
#                                                    region_drop_perc = 0.001,
#                                                    drop_classes = c("Ponderosa Pine Forest, Woodland and Savanna",
#                                                                     "Big Sagebrush Shrubland and Steppe"),
#                                                    drop_classes_column_name = "EVT_GP_N")
# toc()
# 
# 
# 
# 
# 
# tic()
# test4 <- representative_categorical_cover_analysis(raster = raster,
#                                                    raster_cat_df = raster_cats,
#                                                    region_shape = region_shape,
#                                                    aoi_shape = aoi_shape,
#                                                    cat_base_column_name = "VALUE",
#                                                    region_drop_perc = 0,
#                                                    aoi_drop_perc = 0,
#                                                    drop_classes = NA,
#                                                    drop_classes_column_name = NA)
# toc()
# 
# terra::plot(test4$raster_represented)
# terra::plot(test4$raster_not_represented)
# 
# terra::writeRaster(test4$lc_analysis_raster, here::here(dir_derived, "test.tif"), overwrite = TRUE)
# 
# 
# freq(test4$lc_analysis_raster)
# 
# 
# sum(test4$df_represented$region_count)
# 




