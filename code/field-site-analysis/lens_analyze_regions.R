

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
    "tigris",
    "tmap",
    "stars"),
  auto_install = "y"
)

# Set up necessary data directories
dir_ensure(here::here("data"))
dir_raw <- here::here("data/raw")
dir_derived <- here::here("data/derived")
dir_figs <- here::here("figs")
dir_ensure(dir_raw)
dir_ensure(dir_derived)
dir_ensure(dir_figs)

#For outputs
dir_representative <- here::here(dir_derived, "representative_analyses_lens")
dir_ensure(dir_representative)


options(timeout = 1500)


#Download and stream data sources
raster <- access_landfire_evt_conus_2022(access = 'download',
                                         dir_path = dir_raw)
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



# A specific version of representative_categorical_cover_analysis that operates over a full set of matched
# region and AOI polygons, exports results, creates a csv summary, and a single CONUS-wide graphic
conus_lens_analysis <- function(region_polygons_merged,
                                areas_of_interest_merged,
                                raster,
                                raster_cat_df,
                                run_name = "neon_domains",
                                cat_base_column_name,
                                aoi_drop_perc = NA,
                                drop_classes = NA,
                                drop_classes_column_name = NA) {
  
  # Setup output directory for rasters
  clean_aoi_dp <- gsub("\\.", "", as.character(aoi_drop_perc))
  dir_out <- here::here(dir_representative, paste(run_name, clean_aoi_dp, sep = "_"))
  dir_ensure(dir_out)
  
  #Run analysis using representative_categorical_cover_analysis function
  all_region_results <- purrr::pmap(list(region_shape = split(region_polygons_merged, seq(nrow(region_polygons_merged))),
                                         aoi_shape = split(areas_of_interest_merged, seq(nrow(areas_of_interest_merged))),
                                         run_name = paste(run_name, region_polygons_merged$DomainName, sep ="_")),
                                    representative_categorical_cover_analysis,
                                    raster = raster,
                                    raster_cat_df = raster_cat_df,
                                    cat_base_column_name = "VALUE",
                                    region_drop_perc = 0,
                                    aoi_drop_perc = aoi_drop_perc,
                                    drop_classes = drop_classes,
                                    drop_classes_column_name = drop_classes_column_name,
                                    out_rast_values = "PERC_COVER",
                                    out_rast_type = "BOTH",
                                    out_dir = dir_out,
                                    new_sub_dir = TRUE)

  # Create a dataframe with all percentages and export
  result_df <- purrr::map_dfr(all_region_results, ~ tibble(
    region_name = .x$analysis_name,
    perc_area_not_represented = .x$perc_area_not_represented
  ))
  readr::write_csv(result_df, here::here(dir_out, "neon_domains_results.csv"))
  
  # Read in the list of tif files to create the CONUS figure
  not_rep_tif_files <- list.files(dir_out,
                                  pattern = "not_rep",
                                  full.names = TRUE,
                                  recursive = TRUE)
  
  # CREATE FIGURE AND SAVE
  tmap_options(max.raster = c(plot = 1e7, view = 1e5))
  conus <- tigris::states(cb = TRUE) |>  # `cb = TRUE` for a simplified "cartographic boundary" version
    dplyr::filter(!STUSPS %in% c("HI", "AK", "GU", "VI", "MP", "AS", "PR")) |>
    sf::st_transform(crs = terra::crs(terra::rast(not_rep_tif_files[1])))
  
  # Loop through each raster file, simplify if needed, and add to the tmap object
  for (raster_path in not_rep_tif_files) {
    
    # Load the raster
    r <- stars::read_stars(raster_path,
                           proxy = TRUE)
    
    if(raster_path == not_rep_tif_files[1]) {
      tm_plot <- 
        tm_shape(region_polygons_merged |>
                   sf::st_transform(terra::crs(terra::rast(not_rep_tif_files[1]))),
                 bbox = sf::st_bbox(conus)) +
        tm_borders(col = "gray90", lwd = 1) +
        tm_fill(col = "gray90") +
        tmap::tm_shape(r,
                       bbox = sf::st_bbox(conus),
                       downsample = TRUE) +
        tmap::tm_raster(palette = "YlOrRd",
                        style = "cont",
                        breaks = c(0, 15),
                        legend.show = TRUE,
                        title = "Unrepresented landscape\npercentage by class",
                        legend.reverse = FALSE,
                        legend.format = list(fun = function(x) paste0(x, "%")),
                        legend.is.portrait = FALSE)
    } else {
      # Add the raster to the tmap object
      tm_plot <- tm_plot +
        tmap::tm_shape(r,
                       bbox = sf::st_bbox(conus),
                       downsample = TRUE) +
        tmap::tm_raster(palette = "YlOrRd",
                        style = "cont",
                        breaks = c(0, 15),
                        legend.show = FALSE)
    }
  }
  
  # Finalize the plot layout with the legend outside
  tm_plot <- tm_plot +
    tm_shape(region_polygons_merged) +
    tm_borders(col = "gray20",
               lwd = 1) +
    tm_fill(col = NA, alpha = 0) +
    tm_shape(areas_of_interest_merged) +
    tm_borders(col = "darkblue",
               lwd = 1) +
    tmap::tm_layout(legend.outside = FALSE,
                    legend.position = c("left", "bottom"))
  
  # Display the plot
  tmap::tmap_save(tm_plot, here::here(dir_figs, paste(run_name, clean_aoi_dp, ".jpeg", sep = "_")))
  
}

conus_lens_analysis(region_polygons_merged = region_polygons_merged,
                    areas_of_interest_merged = areas_of_interest_merged,
                    raster = raster,
                    raster_cat_df = raster_cats,
                    run_name = "neon_domains_evt_raw_all",
                    cat_base_column_name = "VALUE",
                    aoi_drop_perc = 0.001,
                    drop_classes = NA,
                    drop_classes_column_name = NA)






















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
dir_evt0001 <- here::here(dir_representative, "evtraw_adp0001_rdp0")
dir_ensure(dir_evt0001)
all_region_results <- purrr::pmap(list(region_shape = split(region_polygons_merged, seq(nrow(region_polygons_merged))),
                                       aoi_shape = split(areas_of_interest_merged, seq(nrow(areas_of_interest_merged))),
                                       run_name = paste0("neon_domains_",region_polygons_merged$DomainName)),
                                  representative_categorical_cover_analysis,
                                  raster = raster,
                                  raster_cat_df = raster_cats,
                                  cat_base_column_name = "VALUE",
                                  region_drop_perc = 0,
                                  aoi_drop_perc = 0.001,
                                  drop_classes = NA,
                                  drop_classes_column_name = NA,
                                  out_rast_values = "PERC_COVER",
                                  out_rast_type = "BOTH",
                                  out_dir = dir_evt0001,
                                  new_sub_dir = TRUE)
toc() #just under 4 hours

# Create a dataframe with all percentages and export
result_df <- purrr::map_dfr(all_region_results, ~ tibble(
  region_name = .x$analysis_name,
  perc_area_not_represented = .x$perc_area_not_represented
))
readr::write_csv(result_df, here::here(dir_evt0001, "neon_domains_results.csv"))


not_rep_tif_files <- list.files(dir_evt0001,
                                pattern = "not_rep",
                                full.names = TRUE,
                                recursive = TRUE)







tic()
tmap_options(max.raster = c(plot = 1e8, view = 1e5))
conus <- tigris::states(cb = TRUE) |>  # `cb = TRUE` for a simplified "cartographic boundary" version
  dplyr::filter(!STUSPS %in% c("HI", "AK", "GU", "VI", "MP", "AS", "PR")) |>
  sf::st_transform(crs = terra::crs(terra::rast(not_rep_tif_files[1])))


# Loop through each raster file, simplify if needed, and add to the tmap object
for (raster_path in not_rep_tif_files) {

  # Load the raster
  r <- stars::read_stars(raster_path,
                         proxy = TRUE)
  
  if(raster_path == not_rep_tif_files[1]) {
    tm_plot <- 
      tm_shape(region_polygons_merged |>
                 sf::st_transform(terra::crs(terra::rast(not_rep_tif_files[1]))),
               bbox = sf::st_bbox(conus)) +
      tm_borders(col = "gray90", lwd = 1) +
      tm_fill(col = "gray90") +
      tmap::tm_shape(r,
                                  bbox = sf::st_bbox(conus),
                                  downsample = TRUE) +
      tmap::tm_raster(palette = "YlOrRd",
                      style = "cont",
                      breaks = c(0, 15),
                      legend.show = TRUE,
                      title = "Unrepresented landscape\npercentage by class",
                      legend.reverse = FALSE,
                      legend.format = list(fun = function(x) paste0(x, "%")),
                      legend.is.portrait = FALSE)
  } else {
    # Add the raster to the tmap object
    tm_plot <- tm_plot +
      tmap::tm_shape(r,
                     bbox = sf::st_bbox(conus),
                     downsample = TRUE) +
      tmap::tm_raster(palette = "YlOrRd",
                      style = "cont",
                      breaks = c(0, 15),
                      legend.show = FALSE)
  }
}

# Finalize the plot layout with the legend outside
tm_plot <- tm_plot +
  tm_shape(region_polygons_merged) +
  tm_borders(col = "gray20",
              lwd = 1) +
  tm_fill(col = NA, alpha = 0) +
  tm_shape(areas_of_interest_merged) +
  tm_borders(col = "darkblue",
              lwd = 1) +
  tmap::tm_layout(legend.outside = FALSE,
                  legend.position = c("left", "bottom"))

# Display the plot
tmap::tmap_save(tm_plot, here::here(dir_figs, "testdownsample_big.jpeg"))
toc() #100- 123 seconds with default max.raster; 









# 
# tic()
# conus <- tigris::states(cb = TRUE) |>  # `cb = TRUE` for a simplified "cartographic boundary" version
#   dplyr::filter(!STUSPS %in% c("HI", "AK", "GU", "VI", "MP", "AS", "PR")) |>
#   sf::st_transform(crs = terra::crs(terra::rast(not_rep_tif_files[1])))
# 
# # Loop through each raster file, simplify if needed, and add to the tmap object
# for (raster_path in not_rep_tif_files) {
#   
#   # Load the raster
#   r <- terra::rast(raster_path) |>
#     terra::aggregate(fact = 10, na.rm = TRUE)
#   
#   
#   if(raster_path == not_rep_tif_files[1]) {
#     tm_plot <- 
#       tm_shape(region_polygons_merged |>
#                  sf::st_transform(terra::crs(terra::rast(not_rep_tif_files[1]))),
#                bbox = sf::st_bbox(conus)) +
#       tm_borders(col = "gray90", lwd = 1) +
#       tm_fill(col = "gray90") +
#       tmap::tm_shape(r,
#                      bbox = sf::st_bbox(conus),
#                      downsample = FALSE) +
#       tmap::tm_raster(palette = "YlOrRd",
#                       style = "cont",
#                       breaks = c(0, 15),
#                       legend.show = TRUE,
#                       title = "Unrepresented landscape\npercentage by class",
#                       legend.reverse = FALSE,
#                       legend.format = list(fun = function(x) paste0(x, "%")),
#                       legend.is.portrait = FALSE)
#   } else {
#     # Add the raster to the tmap object
#     tm_plot <- tm_plot +
#       tmap::tm_shape(r,
#                      bbox = sf::st_bbox(conus),
#                      downsample = FALSE) +
#       tmap::tm_raster(palette = "YlOrRd",
#                       style = "cont",
#                       breaks = c(0, 15),
#                       legend.show = FALSE)
#   }
# }
# 
# # Finalize the plot layout with the legend outside
# tm_plot <- tm_plot +
#   tm_shape(region_polygons_merged) +
#   tm_borders(col = "gray20",
#              lwd = 1) +
#   tm_fill(col = NA, alpha = 0) +
#   tm_shape(areas_of_interest_merged) +
#   tm_borders(col = "darkblue",
#              lwd = 1) +
#   tmap::tm_layout(legend.outside = FALSE,
#                   legend.position = c("left", "bottom"))
# 
# # Display the plot
# tmap::tmap_save(tm_plot, here::here(dir_figs, "testaggregate10_big.jpeg"))
# toc() 
# 
# 



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




