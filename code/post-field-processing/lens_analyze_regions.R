

# Setup ----
#rm(list = ls())

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


#Load data & make available for offline analysis
raster_path <- here::here(dir_raw, "LC23_EVT_240.tif")
if(file.exists(raster_path)) {
  raster <- terra::rast(raster_path)
} else {
  tic()
  raster <- access_landfire_evt_conus_2023() # on-campus run time: 250 seconds; cyverse R run time: 164 seconds 
  toc()
  terra::writeRaster(raster, raster_path)
}

evt_cats_path <- here::here(dir_raw, "evt_cats.csv")
if(file.exists(evt_cats_path)) {
  raster_cats <- sf::st_read(evt_cats_path)
} else {
  raster_cats <- access_landfire_evt_conus_2023_csv() 
  readr::write_csv(raster_cats, evt_cats_path)
}

neon_domains_path <- here::here(dir_raw, "neon_domains.gpkg")
if(file.exists(neon_domains_path)) {
  region_polygons <- sf::st_read(neon_domains_path)
} else {
  region_polygons <- access_neon_domains_shp()
  sf::st_write(region_polygons, neon_domains_path)
}
#region_polygons <- access_data_epa_l3_ecoregions_vsi()

aop_flight_box_path <- here::here(dir_raw, "aop_flight_boxes.gpkg")
if(file.exists(aop_flight_box_path)) {
  areas_of_interest <- sf::st_read(aop_flight_box_path)
} else {
  areas_of_interest <- access_neon_aop_flight_box_data() #note that flight boxes have the domain data as "D##" instead of just "##"
  sf::st_write(areas_of_interest, aop_flight_box_path)
}

areas_of_interest <- areas_of_interest |>
  sf::st_transform(sf::st_crs(region_polygons))



representative_categorical_cover_analysis <- function(raster,
                                                      raster_cat_df,
                                                      region_shape,
                                                      aoi_shape,
                                                      region_name = "Not Provided",
                                                      cat_base_column_name, 
                                                      aoi_drop_perc = NA,
                                                      region_drop_perc = NA,
                                                      drop_classes = NA,
                                                      drop_classes_column_name = NA,
                                                      return_rast_values_as_landscape_perc = FALSE) {
  
  # Crop sub-regions for analysis
  print('Cropping to region')
  larger_region_cover <- crop_careful_universal(raster = raster, vector = region_shape, mask = TRUE, verbose = FALSE) 
  print(paste0('Cropping to sub-region aoi'))
  aoi_cover <- crop_careful_universal(raster = larger_region_cover, vector = aoi_shape, mask = TRUE, verbose = FALSE)
  
  # Analyze
  landcover_analysis_output_raw <- analyze_categorical_cover(aoi_raster = aoi_cover,
                                                         larger_region_raster = larger_region_cover,
                                                         raster_cat_df = raster_cat_df,
                                                         cat_base_column = cat_base_column_name)
  
  landcover_analysis_output_included <- landcover_analysis_output_raw
  
  # Remove any classes that are below the regional drop percentage, if specified
  # This is to keep data clean for rasters with many categories that are not common on the landscape
  if(!is.na(region_drop_perc)) {
    landcover_analysis_output_included <- landcover_analysis_output_included |>
      dplyr::filter(region_perc > region_drop_perc)
  }
  
  # Remove any specifically called out classes
  if (length(drop_classes) > 0 && !all(is.na(drop_classes))) {
    drop_classes_col_sym <- rlang::sym(drop_classes_column_name)
    landcover_analysis_output_included <- landcover_analysis_output_included |>
      dplyr::filter(!(.data[[drop_classes_column_name]] %in% drop_classes))
  }
  
  # Remove classes that are below a certain AOI percentage (i.e. that are not adequately represented within the aoi)
  if(!is.na(aoi_drop_perc)) {
    df_represented <- landcover_analysis_output_included |>
      dplyr::filter(aoi_perc > aoi_drop_perc)
    df_not_represented <- landcover_analysis_output_included |>
      dplyr::filter(aoi_perc <= aoi_drop_perc)
  }
  
  # Generate new raster and return data frame AND new raster
  print('Generating new rasters')
  raster_not_represented <- keep_tif_values_in_df(raster = larger_region_cover, df = df_not_represented)
  raster_represented <- keep_tif_values_in_df(raster = larger_region_cover, df = df_represented)
  perc_area_not_represented <- (sum(df_not_represented$region_count) / sum(landcover_analysis_output_included$region_count)) * 100

  if(return_rast_values_as_landscape_perc) {
    print("Reclassifying output rasters to use landscape percentage instead of raw landcover values")
    not_rep_classify <- df_not_represented |>
      dplyr::select({{cat_base_column_name}}, region_perc) |>
      as.matrix()
    raster_not_represented <- raster_not_represented |>
      terra::classify(not_rep_classify)

    rep_classify <- df_represented |>
      dplyr::select({{cat_base_column_name}}, region_perc) |>
      as.matrix()
    raster_represented <- raster_represented |>
      terra::classify(rep_classify)
  } else {print("Returning output rasters with raw landcover values (default behavior)")}
  
  return(list(analysis_name = region_name,
              df_raw = landcover_analysis_output_raw,
              df_included = landcover_analysis_output_included,
              df_represented = df_represented,
              df_not_represented = df_not_represented,
              raster_represented = raster_represented,
              raster_not_represented = raster_not_represented,
              perc_area_not_represented = perc_area_not_represented))
}


## Function to analyze landcover comparisons between AOI and a larger region.
# It will return a dataframe with columns for region_cover and aoi_cover
# both raw and in percentage of the total region and percentage of the aoi.
# The function will join the raster categorical data to the frequencies,
# and group the data and summarize it if desired based on a column of interest 
#aoiCover - land cover raster for the smaller region of interest
#regionCover - land cover raster for a region
# group: whether to group the data or not (TRUE, FALSE) - note that if grouped, all other columns from cats will be dropped, and the output will no longer have the associated raster values
analyze_categorical_cover <- function(aoi_raster, larger_region_raster, raster_cat_df, cat_base_column_name, group = FALSE, cat_group_column_name = NA) {
  
  #Get frequencies & ensure that freq tables are clean
  print("Getting frequencies")
  aoi_freq <- terra::freq(aoi_raster) |>
    dplyr::select(-layer) |>
    condense_freq_groups()
  region_freq <- terra::freq(larger_region_raster) |>
    dplyr::select(-layer) |>
    condense_freq_groups()
  
  # Join together
  all_freqs <- region_freq |>
    dplyr::full_join(aoi_freq, by = c("value")) |>
    dplyr::mutate_if(is.numeric, coalesce, 0) |> #remove NAs
    dplyr::filter(value != 0) # remove background
  
  
  cat_base_column_sym <- rlang::sym(cat_base_column_name)
  
  names(all_freqs) <- c(cat_base_column_name,
                        "region_count",
                        "aoi_count")

  # Join raster cat codes
  raster_cat_df <- raster_cat_df |>
    dplyr::mutate({{cat_base_column_sym}} := as.numeric({{cat_base_column_sym}}))
  
  all_freqs <- all_freqs |>
    dplyr::left_join(raster_cat_df, by = cat_base_column_name)
  
  
  # Perform grouping summarization on group of choice if desired
  if(group) {
    cat_group_column_sym <- rlang::sym(cat_group_column_name)
    all_freqs <- all_freqs |>
      dplyr::group_by({{cat_group_column_sym}}) |>
      dplyr::summarise(region_count = sum(region_count),
                       aoi_count = sum(aoi_count)) |>
      dplyr::ungroup()
  }
  

  # Add percentages and differences for analysis
  print("Calculating percentages")
  all_freqs <- all_freqs |>
    dplyr::mutate(region_perc = 100 * (region_count / sum(all_freqs$region_count)),
                  aoi_perc = 100 * (aoi_count / sum(all_freqs$aoi_count)),
                  diff_in_perc = region_perc - aoi_perc,
                  diff_in_num = region_count - aoi_perc)
  
  # Arrange and return
  all_freqs <- all_freqs |>
    dplyr::arrange(dplyr::desc(diff_in_perc))
  
  return(all_freqs)
}




# A function to create the new raster; all values in df will now be NA
# TIF must contain a column titled "VALUE"
keep_tif_values_in_df <- function(raster, df) {
  values_not_in_df <- !terra::values(raster) %in% df$VALUE
  new_raster <- raster
  values(new_raster)[values_not_in_df] <- NA
  
  rm(values_not_in_df)
  
  #Maintain old cats
  c <- terra::cats(raster)[[1]]
  levels(new_raster) <- c
  
  return(new_raster)
}


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
  dplyr::summarise(geom = sf::st_union(geom)) |>
  dplyr::ungroup() |>
  dplyr::arrange(DomainID)

areas_of_interest_merged <- areas_of_interest |>
  sf::st_make_valid() %>% #force validity, duplicate vertex error
  dplyr::filter(sf::st_intersects(., conus, sparse = FALSE)) |>
  dplyr::group_by(domain, domainName) |>
  dplyr::summarise(geom = sf::st_union(geom)) |>
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


tic()
results <- representative_categorical_cover_analysis(raster = raster,
                                                     raster_cat_df = raster_cats,
                                                     region_shape = region_polygons_merged[1,],
                                                     aoi_shape = areas_of_interest_merged[1,],
                                                     region_name = "TEST",
                                                     cat_base_column_name = "VALUE",
                                                     region_drop_perc = 0,
                                                     aoi_drop_perc = 0,
                                                     drop_classes = NA,
                                                     drop_classes_column_name = NA)
toc()


all_region_results <- purrr::pmap(list(region_shape = region_polygons_merged,
                                       aoi_shape = areas_of_interest_merged,
                                       region_name = region_polygons_merged$DomainName),
                                  representative_categorical_cover_analysis(),
                                  raster = raster,
                                  raster_cat_df = raster_cats,
                                  cat_base_column_name = "VALUE",
                                  region_drop_perc = 0,
                                  aoi_drop_perc = 0,
                                  drop_classes = NA,
                                  drop_classes_column_name = NA)










# TESTING


#Southern rockies
region_shape <- region_polygons |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = st_union(geometry)) |>
  dplyr::ungroup()

aoi_shape <- areas_of_interest |>
  dplyr::filter(siteID == "NIWO")
  
tic()
results <- representative_categorical_cover_analysis(raster = raster,
                                                     raster_cat_df = raster_cats,
                                                     region_shape = region_shape,
                                                     aoi_shape = aoi_shape,
                                                     region_name = "TEST",
                                                     cat_base_column_name = "VALUE",
                                                     region_drop_perc = 0,
                                                     aoi_drop_perc = 0.001,
                                                     drop_classes = NA,
                                                     drop_classes_column_name = NA,
                                                     return_rast_values_as_landscape_perc = TRUE)
toc() #on-campus run time:


terra::writeRaster(results$raster_not_represented, here::here(dir_derived, "test_sr_notrep_perc.tif"))




# Crop sub-regions for analysis
print(paste0('Cropping to region', region_name))
tic()
larger_region_raster <- crop_careful_universal(raster = raster, vector = region_shape, mask = TRUE, verbose = TRUE) 
toc()
tic()
aoi_raster <- crop_careful_universal(raster = larger_region_raster, vector = aoi_shape, mask = TRUE, verbose = TRUE)
toc()

# Analyze
landcover_analysis_output <- analyze_categorical_cover(aoi_raster = aoi_raster,
                                               larger_region_raster = larger_region_raster,
                                               raster_cat_df = raster_cats,
                                               cat_base_column = "VALUE")

landcover_analysis_output2 <- analyze_categorical_cover(aoi_raster = aoi_raster,
                                                       larger_region_raster = larger_region_raster,
                                                       raster_cat_df = raster_cats,
                                                       cat_base_column = "VALUE",
                                                       group = TRUE,
                                                       cat_group_column = "EVT_GP_N")




tic()
test <- representative_categorical_cover_analysis(raster = raster,
                                                  raster_cat_df = raster_cats,
                                                  region_shape = region_shape,
                                                  aoi_shape = aoi_shape,
                                                  cat_base_column_name = "VALUE",
                                                  group = TRUE,
                                                  cat_group_column_name = "EVT_GP_N",
                                                  region_drop_perc = NA,
                                                  drop_classes = NA,
                                                  drop_classes_column_name = NA)
toc()




tic()
test2 <- representative_categorical_cover_analysis(raster = raster,
                                                  raster_cat_df = raster_cats,
                                                  region_shape = region_shape,
                                                  aoi_shape = aoi_shape,
                                                  cat_base_column_name = "VALUE",
                                                  group = TRUE,
                                                  cat_group_column_name = "EVT_GP_N",
                                                  region_drop_perc = 0.001,
                                                  drop_classes = NA,
                                                  drop_classes_column_name = NA)
toc()




tic()
test3 <- representative_categorical_cover_analysis(raster = raster,
                                                   raster_cat_df = raster_cats,
                                                   region_shape = region_shape,
                                                   aoi_shape = aoi_shape,
                                                   cat_base_column_name = "VALUE",
                                                   group = TRUE,
                                                   cat_group_column_name = "EVT_GP_N",
                                                   region_drop_perc = 0.001,
                                                   drop_classes = c("Ponderosa Pine Forest, Woodland and Savanna",
                                                                    "Big Sagebrush Shrubland and Steppe"),
                                                   drop_classes_column_name = "EVT_GP_N")
toc()





tic()
test4 <- representative_categorical_cover_analysis(raster = raster,
                                                   raster_cat_df = raster_cats,
                                                   region_shape = region_shape,
                                                   aoi_shape = aoi_shape,
                                                   cat_base_column_name = "VALUE",
                                                   region_drop_perc = 0,
                                                   aoi_drop_perc = 0,
                                                   drop_classes = NA,
                                                   drop_classes_column_name = NA)
toc()

terra::plot(test4$raster_represented)
terra::plot(test4$raster_not_represented)

terra::writeRaster(test4$lc_analysis_raster, here::here(dir_derived, "test.tif"), overwrite = TRUE)


freq(test4$lc_analysis_raster)


sum(test4$df_represented$region_count)













read_spatial_subset_local_raster <- function(path, shp) {
  bbox <- 
  
  
}



















