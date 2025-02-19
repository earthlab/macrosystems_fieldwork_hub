# Tyler L. McIntosh

# Landscape representativeness ----

# Function to get presence of a raster value within NEON base plots
# PARAMETERS
# siteID :: the NEON ID of interest (e.g. "YELL")
# cat_raster :: a raster to pull values from (intended to be a landcover categorical raster), in SpatRaster format
get_base_plot_freqs <- function(site_id, cat_raster) {
  epsg <- terra::crs(cat_raster)
  plots <- access_neon_plots_shp() |>
    sf::st_transform(epsg)
  base_plots <- plots |>
    dplyr::filter(subtype == "basePlot" & site_id == site_id)
  crop_rast <- cat_raster |>
    terra::crop(y = base_plots, mask = TRUE, touches = FALSE)
  freqs <- terra::freq(crop_rast)  
  
  return(freqs)
}


#' Condense Frequency Table Groups
#'
#' This function takes a frequency table, typically returned by `terra::freq()`, and condenses
#' rows if there are duplicate values by summing their associated counts. If the table already 
#' contains unique values, no changes are made.
#'
#' @param freq_dats A data frame or tibble returned by `terra::freq()` that contains two columns: 
#'   `value` (the value names) and `count` (the corresponding frequency of each value).
#'
#' @return A condensed version of the frequency table where duplicate values have been merged 
#'   and their counts summed. If no duplicates are found, the original table is returned.
#' 
#' @importFrom dplyr group_by summarise ungroup
#'
#' @examples
#' # Example usage with a mock frequency table
#' freq_table <- data.frame(
#'   value = c(1, 1, 2, 3, 3, 3),
#'   count = c(10, 15, 5, 2, 8, 6)
#' )
#' 
#' condense_freq_groups(freq_table)
#'
#' @export
condense_freq_groups <- function(freq_dats) {
  if (length(unique(freq_dats$value)) == length(freq_dats$value)) {
    print('Frequency table is already correct')
    return(freq_dats)
  } else {
    print('Condensing frequency table')
    freq_dats <- freq_dats %>%
      group_by(value) %>%
      summarise(count = sum(count)) %>%
      ungroup()
    return(freq_dats)
  }
}



#out_rast_values = "BOTH", "PERC_COVER", or "RAW"
#out_rast_type = "BOTH", "REP", "NOT_REP", or "NONE"
representative_categorical_cover_analysis <- function(raster,
                                                      raster_cat_df,
                                                      region_shape,
                                                      aoi_shape,
                                                      run_name = "NotProvided",
                                                      cat_base_column_name, 
                                                      aoi_drop_perc = NA,
                                                      region_drop_perc = NA,
                                                      drop_classes = NA,
                                                      drop_classes_column_name = NA,
                                                      out_rast_values = "BOTH",
                                                      out_rast_type = "BOTH",
                                                      out_dir = "",
                                                      new_sub_dir = FALSE) {
  
  print(paste0("Operating on run: ", run_name))
  
  # Validate inputs
  if(!out_rast_values %in% c("BOTH", "PERC_COVER", "RAW")) {
    stop("Invalid value for 'out_rast_values'. Must be one of: 'BOTH', 'PERC_COVER', 'RAW'.")
  }
  
  if(!out_rast_type %in% c("BOTH", "REP", "NOT_REP", "NONE")) {
    stop("Invalid value for 'out_rast_values'. Must be one of: 'BOTH', 'REP', 'NOT_REP', 'NONE'.")
  }
  
  if(run_name == "NotProvided") {
    warning("You have not provided a run_name; 'NotProvided' will be used")
  }
  
  if(!cat_base_column_name %in% names(raster_cat_df)) {
    stop("cat_base_column_name must be one of the column names in raster_cat_df")
  }
  
  
  #Setup output directories
  clean_run_name <- run_name %>%
    gsub(" ", "", .) %>%
    gsub("/", "_", .)
  clean_aoi_dp <- gsub("\\.", "", as.character(aoi_drop_perc))
  clean_region_dp <- gsub("\\.", "", as.character(region_drop_perc))
  clean_run_name <- paste(clean_run_name, "_adp", clean_aoi_dp, "_rdp", clean_region_dp, sep = "")
  
  if(new_sub_dir) {
    out_dir <- here::here(out_dir, clean_run_name)
    dir_ensure(out_dir)
  }
  
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
  
  # OLD VERSION
  # #Get percentage of area not represented (only taking into account areas included)
  # perc_area_not_represented <- (sum(df_not_represented$region_count) / sum(landcover_analysis_output_included$region_count)) * 100
  
  
  # NEW VERSION
  not_rep_count <- df_not_represented |>
    dplyr::select(cat_base_column_name, region_count, aoi_count, region_perc, aoi_perc) |>
    dplyr::distinct() |>
    dplyr::pull(region_count) |>
    sum()
  
  all_count <- landcover_analysis_output_included |>
    dplyr::select(cat_base_column_name, region_count, aoi_count, region_perc, aoi_perc) |>
    dplyr::distinct() |>
    dplyr::pull(region_count) |>
    sum()
  

  perc_area_not_represented <- (not_rep_count / all_count) * 100
  #
  
  
  
  # Generate new raster data requested
  
  #Not represented areas
  if(out_rast_type == "BOTH" | out_rast_type == "NOT_REP") {
    print('Generating new rasters: Raster not represented')
    raster_not_represented <- keep_tif_values_in_df(raster = larger_region_cover, df = df_not_represented)
    
    if(out_rast_values == "RAW" | out_rast_values == "BOTH") {
      terra::writeRaster(raster_not_represented,
                         here::here(out_dir, paste0(clean_run_name, "_not_rep_raw.tif")),
                         overwrite = TRUE)
    }
    
    if(out_rast_values == "BOTH" | out_rast_values == "PERC_COVER") {
      print("Reclassifying output raster to use landscape percentage")
      not_rep_classify <- df_not_represented |>
        dplyr::select({{cat_base_column_name}}, region_perc) |>
        as.matrix()
      raster_not_represented <- raster_not_represented |>
        terra::classify(not_rep_classify)
      
      terra::writeRaster(raster_not_represented,
                         here::here(out_dir, paste0(clean_run_name, "_not_rep_perc_cover.tif")),
                         overwrite = TRUE)
    }
    
    rm(raster_not_represented)
    gc()
  }
    
  # represented areas
  if(out_rast_type == "BOTH" | out_rast_type == "REP") {
    
    print('Generating new rasters: Raster represented')
    raster_represented <- keep_tif_values_in_df(raster = larger_region_cover, df = df_represented)
    
    if(out_rast_values == "RAW" | out_rast_values == "BOTH") {
      terra::writeRaster(raster_represented,
                         here::here(out_dir, paste0(clean_run_name, "_rep_raw.tif")),
                         overwrite = TRUE)
    }
    
    if(out_rast_values == "BOTH" | out_rast_values == "PERC_COVER") {
      print("Reclassifying output raster to use landscape percentage")
      
      rep_classify <- df_represented |>
        dplyr::select({{cat_base_column_name}}, region_perc) |>
        as.matrix()
      raster_represented <- raster_represented |>
        terra::classify(rep_classify)
      
      terra::writeRaster(raster_represented,
                         here::here(out_dir, paste0(clean_run_name, "_rep_perc_cover.tif")),
                         overwrite = TRUE)
    }
    
    rm(raster_represented)
    gc()
  
  }
  
  # no rasters
  if(out_rast_type == "NONE") {
    print("You have not requested a written output raster, returning only in-memory non-spatial data")
  }
  
  return(list(analysis_name = run_name,
              df_raw = landcover_analysis_output_raw,
              df_included = landcover_analysis_output_included,
              df_represented = df_represented,
              df_not_represented = df_not_represented,
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
  
  # THIS IS NEW #
  #Ensure that raster categories and active category are set correctly
  levels(aoi_raster) <- raster_cat_df
  terra::activeCat(aoi_raster) <- cat_base_column_name
  levels(larger_region_raster) <- raster_cat_df
  terra::activeCat(larger_region_raster) <- cat_base_column_name
  # # #
  
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
  
  #Ensure both columns are numeric
  raster_cat_df <- raster_cat_df |>
    dplyr::mutate({{cat_base_column_sym}} := as.numeric({{cat_base_column_sym}}))
  all_freqs <- all_freqs |>
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
  # values_not_in_df <- !terra::values(raster) %in% df$VALUE
  # new_raster <- raster
  # values(new_raster)[values_not_in_df] <- NA
  # 
  # rm(values_not_in_df)
  
  new_raster <- terra::ifel(raster %in% df$VALUE, raster, NA)
  
  #Maintain old cats
  c <- terra::cats(raster)[[1]]
  levels(new_raster) <- c
  
  return(new_raster)
}

# Generating plots ----


# A function to randomly generate aop locations for Macrosystems
# PARAMETERS
# aop :: the aop area(s) of interest, as an sf object
# lc :: the landcover file to use for selections, as a SpatRaster
# numPlotsEach :: the number of plots to select for each landcover type present in the lc file
# name :: the human-readable name of the function run, which will be added to file paths (e.g. "Bend")
# lcName :: the human-readable name of the landcover tif (e.g. evt5window)
# outDir :: the directory to output data to
# sma :: the surface management agency. "USFS" or other string
generate_aop_plots <- function(aop,
                               lc,
                               numPlotsEach,
                               name,
                               lcName,
                               outDir,
                               sma = "USFS",
                               distFromRoadsMax) {
  
  epsg <- "EPSG:5070" #Albers equal area
  
  
  #Create operating area
  reasonableAreasOfAccess <- sf::st_read(here::here("data/manual/reasonable_areas_of_access.gpkg")) |>  #these are manually created areas of access. For YELL it is from create_yell_neon_aoi.R
    sf::st_transform(epsg) |>
    dplyr::mutate(grp = 1) |>
    dplyr::group_by(grp) |>
    dplyr::summarise(geom = st_union(geom)) |>
    dplyr::ungroup()
  
  genOpArea <- aop |>
    sf::st_transform(epsg) |>
    dplyr::mutate(group = 1) |>
    dplyr::group_by(group) |>
    dplyr::summarise(geometry = st_union(geometry)) |>
    dplyr::ungroup() |>
    sf::st_buffer(-50) |>
    sf::st_intersection(reasonableAreasOfAccess) |>
    dplyr::select(-group, -grp)
  
  
  #Access slope
  slope <- access_us_slope(slopeF = here::here('data/raw', 'usa_slope.tif')) #DEM Data
  # Clip slope
  thisSlope <- slope |>
    crop_careful_universal(aop, mask = TRUE, verbose = FALSE) |>
    terra::project(epsg)
  
  #Access roads
  roads <- access_osm_roads(aoi = genOpArea)
  
  
  #Buffer roads
  bigRoadBuff <- roads |> sf::st_buffer(distFromRoadsMax)
  
  #Surface Management Agency (SMA)
  
  if(sma == "USFS") {
    usfs <- access_us_sma(dir_raw,
                          layer = "SurfaceMgtAgy_USFS") |>
      sf::st_transform(epsg)
    
    thisUsfs <- usfs |>
      sf::st_filter(genOpArea) |>
      dplyr::mutate(group = 1) |>
      dplyr::group_by(group) |>
      dplyr::summarise(SHAPE = st_union(SHAPE)) |>
      dplyr::ungroup() |>
      sf::st_buffer(-400) |>
      sf::st_intersection(genOpArea)
  }
  
  
  
  #Clip land cover data
  evtHere <- lc |>
    crop_careful_universal(vector = genOpArea,
                           mask = TRUE,
                           verbose = FALSE) |>
    terra::project(epsg)
  
  
  #Slope mask
  thisSlopeResample <- thisSlope |>
    terra::crop(evtHere,
                snap = "near",
                extend = TRUE) |>
    terra::resample(evtHere,
                    method = "near") |>
    terra::project(epsg)
  slopeMask <- thisSlopeResample > 15
  
  
  #Apply all masks and generate sampling options raster
  evtHereMasked <- evtHere |>
    terra::mask(mask = slopeMask,
                maskvalues = TRUE) |>
    terra::mask(bigRoadBuff)
  
  if(sma == "USFS") {
    evtHereMasked <- evtHereMasked |>
      terra::mask(thisUsfs)
  }
  
  evtCsv <- access_landfire_evt_conus_2022_csv() |>
    dplyr::mutate(VALUE = as.integer(VALUE))
  
  # Sample raster
  set.seed(1)
  potentialPlots <- raster::sampleStratified(raster::raster(evtHereMasked), size = numPlotsEach, na.rm = TRUE, sp = TRUE) |>
    sf::st_as_sf() |>
    dplyr::mutate(VALUE = EVT_NAME) |>
    dplyr::select(-EVT_NAME) |>
    dplyr::left_join(evtCsv, by = join_by(VALUE == VALUE)) |>
    dplyr::filter(EVT_GP_N != "Open Water" & !grepl("developed", EVT_PHYS, ignore.case = TRUE) & EVT_GP_N != "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads") |>
    dplyr::mutate(plotID = paste0(name, "_", row_number())) |>
    dplyr::mutate(sampleRaster = lcName)
  
  potentialPlotLocationsWGS <- potentialPlots |>
    sf::st_transform("EPSG:4326") |>
    sf::st_coordinates() %>%
    cbind(potentialPlots, .) |>
    dplyr::rename(lat = Y, long = X)
  
  potentialPlotLocationsClean <- potentialPlotLocationsWGS |>
    dplyr::select(plotID, EVT_NAME, lat, long, geometry) |>
    dplyr::rename(landcover = EVT_NAME)
  
  
  #sf::st_write(potentialPlotLocationsWGS, here::here(outDir, glue::glue('aop_{name}_points.gpkg')), append = FALSE)
  sf::st_write(potentialPlotLocationsClean, here::here(outDir, glue::glue('aop_{name}_{lcName}_points.gpkg')), append = FALSE)
  write_csv(potentialPlotLocationsClean |> sf::st_drop_geometry(), here::here(outDir, glue::glue('aop_{name}_{lcName}_points.csv')))
  st_write_shp(shp = potentialPlotLocationsClean,
               location = outDir,
               filename = glue::glue("aop_{name}_{lcName}_points"),
               zip_only = TRUE)
}




# A function to create the general operating area for plot selection
# PARAMETERS
# ranger :: USFS ranger district as an sf spatial object
# ecoRegion :: EPA ecoregion as an sf spatial object
create_operating_area <- function(ranger, ecoRegion) {
  
  epsg <- "EPSG:5070" #Albers equal area
  
  ranger <- ranger |>
    sf::st_transform(epsg)
  
  ecoRegion <- ecoRegion |>
    sf::st_transform(epsg)
  
  reasonableAreasOfAccess <- sf::st_read(here::here("data/manual/reasonable_areas_of_access.gpkg")) |>  #these are manually created areas of access. For YELL it is from create_yell_neon_aoi.R
    sf::st_transform(epsg) |>
    dplyr::mutate(grp = 1) |>
    dplyr::group_by(grp) |>
    dplyr::summarise(geom = st_union(geom)) |>
    dplyr::ungroup()
  
  #Wilderness & WSA
  wildernessAll <- access_us_wilderness(dest_path = here::here('data', 'raw', 'wild.gpkg')) |> 
    sf::st_transform(epsg) |>
    sf::st_buffer(400) #USFS standard is no drones w/in 300m of wilderness boundaries + 70, same reasoning as road buffer; add an extra 30m to be safe = 400
  
  wilderness <- wildernessAll |>
    sf::st_filter(ranger) |>
    dplyr::mutate(group = 1) |>
    dplyr::group_by(group) |>
    dplyr::summarise(geometry = st_union(geometry)) |>
    dplyr::ungroup()
  
  wsaAll <- access_us_wilderness_study_areas(dest_path = here::here('data', 'raw', 'wsa.gpkg')) |>
    sf::st_transform(epsg) |>
    sf::st_buffer(400) #USFS standard is no drones w/in 300m of wilderness boundaries + 70, same reasoning as road buffer; add an extra 30m to be safe = 400
  
  wsa <- wsaAll |>
    sf::st_filter(ranger) |>
    dplyr::mutate(group = 1) |>
    dplyr::group_by(group) |>
    dplyr::summarise(geometry = st_union(geometry)) |>
    dplyr::ungroup()
  
  genOpArea <- ecoRegion |>
    sf::st_buffer(-50) |>
    sf::st_intersection(ranger) |>
    dplyr::mutate(group = 1) |>
    dplyr::group_by(group) |>
    dplyr::summarise(geometry = st_union(geometry)) |>
    dplyr::ungroup() |>
    sf::st_intersection(reasonableAreasOfAccess)
  
  if(nrow(wilderness) > 0) {
    genOpArea <- genOpArea |>
      sf::st_difference(wilderness)
  }
  if(nrow(wsa) > 0) {
    genOpArea <- genOpArea |>
      sf::st_difference(wsa)
  }
  
  return(genOpArea)
}



# Generating AOP tile polygons ----

# Helper function to determine the appropriate NAD83 UTM zone EPSG code for a given polygon
find_nad83_utm_epsg <- function(polygon) {
  
  # Ensure the polygon is in WGS84 (EPSG:4326) for accurate longitude calculation
  polygonWgs84 <- sf::st_transform(polygon, crs = 4326)
  
  # Calculate the centroid
  centroid <- sf::st_centroid(polygonWgs84)
  centroidCoords <- sf::st_coordinates(centroid)
  
  # Get the longitude of the centroid
  longitude <- centroidCoords[1, "X"]
  
  # Calculate the UTM zone
  utmZone <- (floor((longitude + 180) / 6) %% 60) + 1
  
  # Determine the EPSG code for NAD83 UTM zone
  epsgCode <- 26900 + utmZone
  
  return(epsgCode)
}


# A function to generate a gridded set of polygons representing AOP image tiles for a NEON AOP footprint (the given aoi)
generate_aop_tile_polygons <- function(aoi) {
  
  # Find the NAD83 UTM EPSG code for the AOI
  epsgCode <- find_nad83_utm_epsg(aoi)
  
  # Transform the AOI to the appropriate UTM CRS
  aoiUtm <- sf::st_transform(aoi, crs = epsgCode)
  
  # Get the bounding box of the AOI
  bbox <- sf::st_bbox(aoiUtm)
  
  # Define the range of coordinates for the grid
  xCoords <- seq(floor(bbox["xmin"] / 1000) * 1000, ceiling(bbox["xmax"] / 1000) * 1000, by = 1000)
  yCoords <- seq(floor(bbox["ymin"] / 1000) * 1000, ceiling(bbox["ymax"] / 1000) * 1000, by = 1000)
  
  # Create the grid of polygons
  polygons <- list()
  idIndex <- 1
  
  for (x in xCoords) {
    for (y in yCoords) {
      # Create a 1km by 1km polygon
      polygon <- sf::st_polygon(list(matrix(c(x, y, 
                                          x + 1000, y, 
                                          x + 1000, y + 1000, 
                                          x, y + 1000, 
                                          x, y), 
                                        ncol = 2, byrow = TRUE)))
      
      # Create an sf object for the polygon with an id
      polygons[[idIndex]] <- sf::st_sf(id = paste0(x, "_", y), geometry = sf::st_sfc(polygon, crs = epsgCode))
      idIndex <- idIndex + 1
    }
  }
  
  # Combine all polygons into a single sf object using rbind
  grid <- do.call(rbind, polygons)
  
  # Clip the grid to the AOI
  gridClipped <- sf::st_intersection(grid, aoiUtm)
  
  return(gridClipped)
}


# Operational function to create and write out a polygon set with the names of all image tiles within an AOP footprint
# Example use
# 
# yellTest <- generate_aop_image_grid(site = "YELL",
#                                     year = 2023,
#                                     visit = 5)
generate_aop_image_grid <- function(site, year, visit) {
  
  aop_all <- access_neon_aop_flight_box_data()
  
  aop <- aop_all |>
    dplyr::filter(siteID == site)
  
  site_grid <- generate_aop_tile_polygons(aop) |>
    dplyr::mutate(tileName = paste0(year, "_", site, "_", visit, "_", id, "_image"))
  
  return(site_grid)
}




#' Find NAD83 UTM EPSG Code
#'
#' This helper function calculates the appropriate NAD83 UTM EPSG code for a given polygon based 
#' on its centroid's longitude. The polygon is first transformed to WGS84 (EPSG:4326) for accurate 
#' calculation.
#'
#' @param polygon An sf object representing a polygon. The coordinate reference system (CRS) is assumed to be set.
#'
#' @return An integer representing the EPSG code for the corresponding NAD83 UTM zone.
#'
#' @importFrom sf st_transform st_centroid st_coordinates
#' 
#' @export
find_nad83_utm_epsg <- function(polygon) {
  # Ensure the polygon is in WGS84 (EPSG:4326) for accurate longitude calculation
  polygonWgs84 <- sf::st_transform(polygon, crs = 4326)
  
  # Calculate the centroid
  centroid <- sf::st_centroid(polygonWgs84)
  centroidCoords <- sf::st_coordinates(centroid)
  
  # Get the longitude of the centroid
  longitude <- centroidCoords[1, "X"]
  
  # Calculate the UTM zone
  utmZone <- (floor((longitude + 180) / 6) %% 60) + 1
  
  # Determine the EPSG code for NAD83 UTM zone
  epsgCode <- 26900 + utmZone
  
  return(epsgCode)
}


#' Generate AOP Tile Polygons
#'
#' Generates a set of 1km x 1km polygons representing AOP image tiles within the bounding box 
#' of a given Area of Interest (AOI). The AOI is transformed to the appropriate UTM CRS based 
#' on its centroid before the grid of polygons is created.
#'
#' @param aoi An sf object representing the AOI (area of interest) polygon.
#'
#' @return An sf object containing the gridded polygons clipped to the AOI.
#'
#' @importFrom sf st_transform st_bbox st_polygon st_sfc st_sf st_intersection
#' @importFrom dplyr mutate
#' 
#' @export
generate_aop_tile_polygons <- function(aoi) {
  
  # Find the NAD83 UTM EPSG code for the AOI
  epsgCode <- find_nad83_utm_epsg(aoi)
  
  # Transform the AOI to the appropriate UTM CRS
  aoiUtm <- sf::st_transform(aoi, crs = epsgCode)
  
  # Get the bounding box of the AOI
  bbox <- sf::st_bbox(aoiUtm)
  
  # Define the range of coordinates for the grid
  xCoords <- seq(floor(bbox["xmin"] / 1000) * 1000, ceiling(bbox["xmax"] / 1000) * 1000, by = 1000)
  yCoords <- seq(floor(bbox["ymin"] / 1000) * 1000, ceiling(bbox["ymax"] / 1000) * 1000, by = 1000)
  
  # Create the grid of polygons
  polygons <- list()
  idIndex <- 1
  
  for (x in xCoords) {
    for (y in yCoords) {
      # Create a 1km by 1km polygon
      polygon <- sf::st_polygon(list(matrix(c(x, y, 
                                              x + 1000, y, 
                                              x + 1000, y + 1000, 
                                              x, y + 1000, 
                                              x, y), 
                                            ncol = 2, byrow = TRUE)))
      
      # Create an sf object for the polygon with an id
      polygons[[idIndex]] <- sf::st_sf(id = paste0(x, "_", y), geometry = sf::st_sfc(polygon, crs = epsgCode))
      idIndex <- idIndex + 1
    }
  }
  
  # Combine all polygons into a single sf object using rbind
  grid <- do.call(rbind, polygons)
  
  # Clip the grid to the AOI
  gridClipped <- sf::st_intersection(grid, aoiUtm)
  
  return(gridClipped)
}




#' Generate AOP Image Grid
#'
#' Generates and returns a gridded set of polygons representing AOP image tiles for a given NEON site, 
#' year, and visit number. The function retrieves the flight box data, filters it by site, and 
#' then calls \code{\link{generate_aop_tile_polygons}} to generate the grid. Each tile is named according 
#' to the input parameters.
#'
#' @param site A character string representing the NEON site identifier (e.g., "YELL").
#' @param year An integer specifying the year of the AOP data (e.g., 2023).
#' @param visit An integer representing the visit number (e.g., 5).
#'
#' @return An sf object representing the gridded AOP image tiles with names that include the year, site, 
#' visit number, and tile identifier.
#'
#' @importFrom dplyr filter mutate
#' 
#' @examples
#' \dontrun{
#' # Example: Generate AOP image grid for site "YELL", year 2023, and visit 5.
#' yell_image_grid <- generate_aop_image_grid(site = "YELL", year = 2023, visit = 5)
#' 
#' # View the result
#' print(yell_image_grid)
#' 
#' # Plot the generated grid
#' plot(sf::st_geometry(yell_image_grid))
#' }
#' @export
generate_aop_image_grid <- function(site, year, visit) {
  
  # Access the AOP flight box data
  aop_all <- access_neon_aop_flight_box_data()
  
  # Filter the data for the specified site
  aop <- aop_all |> 
    dplyr::filter(siteID == site)
  
  # Generate the tile polygons for the site
  site_grid <- generate_aop_tile_polygons(aop) |>
    dplyr::mutate(tileName = paste0(year, "_", site, "_", visit, "_", id, "_image"))
  
  return(site_grid)
}



# Generate plot boundaries ----

# Function to get the extent of pixels
get_pixel_extent <- function(row, col, raster) {
  cell_res <- terra::res(raster)  # Get cell resolution
  x_min <- terra::ext(raster)$xmin + cell_res[1] * (col - 1)
  y_max <- terra::ext(raster)$ymax - cell_res[2] * (row - 1)
  x_max <- x_min + cell_res[1]
  y_min <- y_max - cell_res[2]
  return(c(x_min, x_max, y_min, y_max))
}



# A function to create a 0&1 checkerboard SpatRaster that perfectly aligns with ARD pixels,
# providing coverage of the area over which plotPoints exists (buffered by an extra 200m)
create.landsat.checkerboard <- function(plotPoints) {
  
  #Get bounding boxes - both projected & in geographic coordinates
  bbox4326 <- sf::st_bbox(plotPoints |>
                            sf::st_buffer(200) |>
                            sf::st_transform("EPSG:4326"))
  
  bboxProj <- sf::st_bbox(plotPoints |>
                            sf::st_buffer(200))
  
  # Search the stac catalog
  stac("https://landsatlook.usgs.gov/stac-server") |>
    get_request()
  ## ###STACCatalog
  ## - id: earth-search-aws
  ## - description: A STAC API of public datasets on AWS
  ## - field(s): stac_version, type, id, title, description, links, conformsTo
  
  collection_formats()
  
  
  # Record start time
  a <- Sys.time()
  
  # Initialize STAC connection
  s = rstac::stac("https://landsatlook.usgs.gov/stac-server")
  
  
  # Search for landsat ARD images within specified bounding box and date range
  items = s |>
    rstac::stac_search(collections = "landsat-c2ard-sr",
                       bbox = c(bbox4326["xmin"],
                                bbox4326["ymin"],
                                bbox4326["xmax"],
                                bbox4326["ymax"]),
                       datetime = "2021-05-01T00:00:00Z/2021-06-01T00:00:00Z") |>
    post_request() |>
    items_fetch(progress = FALSE)
  
  
  
  
  
  # Print number of found items
  length(items$features)
  
  items
  
  
  # Prepare the assets for analysis
  library(gdalcubes)
  assets = c("qa_pixel")
  ard_collection = gdalcubes::stac_image_collection(items$features, asset_names = assets)
  
  b <- Sys.time()
  difftime(b, a)
  
  # Display the image collection
  ard_collection
  
  
  
  #Access the data
  
  # Record start time
  a <- Sys.time()
  
  # Define a specific view on the satellite image collection
  v = gdalcubes::cube_view(
    srs = epsg,
    dx = 30,
    dy = 30,
    dt = "P1M",
    aggregation = "median",
    resampling = "near",
    extent = list(
      t0 = "2021-05-01",
      t1 = "2021-06-01",
      left = bboxProj["xmin"],
      right = bboxProj["xmax"],
      top = bboxProj["ymax"],
      bottom = bboxProj["ymin"]
    )
  )
  
  b <- Sys.time()
  difftime(b, a)
  
  # Display the defined view
  v
  
  
  
  a <- Sys.time()
  
  x <- ard_collection |>
    raster_cube(v) |>
    write_tif() |>
    terra::rast()
  
  x
  
  b <- Sys.time()
  difftime(b, a)
  
  nrows <- nrow(x)
  ncols <- ncol(x)
  
  # Create a matrix with a checkerboard pattern
  checkerboard_matrix <- matrix(NA, nrow = nrows, ncol = ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      checkerboard_matrix[i, j] <- ifelse((i + j) %% 2 == 0, 1, 0)
    }
  }
  
  # Convert the matrix to a raster
  filled_raster <- terra::rast(checkerboard_matrix, crs = terra::crs(x), extent = terra::ext(x))
  
  return(filled_raster)
  
}



# A function to create plot polygons from a raster template and the plot points
create.polygons.from.raster.and.points <- function(raster, plotPoints) {
  
  plotCoords <- sf::st_coordinates(plotPoints)
  
  cell_index <- terra::cellFromXY(raster, plotCoords)
  row <- terra::rowFromCell(raster, cell_index)
  col <- terra::colFromCell(raster, cell_index)
  
  
  # Find extents of neighboring pixels
  neighboring_extents <- list()
  for (i in seq_along(row)) {
    neighboring_extents[[i]] <- list()
    for (j in c(-1, 0, 1)) {
      for (k in c(-1, 0, 1)) {
        neighboring_row <- row[i] + j
        neighboring_col <- col[i] + k
        neighboring_extents[[i]][[length(neighboring_extents[[i]]) + 1]] <- get_pixel_extent(neighboring_row, neighboring_col, raster)
      }
    }
  }
  
  # Convert neighboring extents to a data frame
  neighbor_df <- data.frame()
  for (i in seq_along(neighboring_extents)) {
    for (extent in neighboring_extents[[i]]) {
      neighbor_df <- rbind(neighbor_df, c(i, extent))
    }
  }
  colnames(neighbor_df) <- c("ID", "xmin", "xmax", "ymin", "ymax")
  
  # Aggregate extents by ID and create plot polygons
  plot_extents <- neighbor_df %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(xmin = min(xmin), xmax = max(xmax), ymin = min(ymin), ymax = max(ymax)) %>%
    as.data.frame()
  
  # Create SpatialPolygons from the extents
  plot_polygons <- apply(plot_extents[2:5], 1, function(x) {
    terra::ext(x["xmin"], x["xmax"], x["ymin"], x["ymax"]) |> 
      terra::as.polygons() |> 
      st_as_sf()
  })
  
  # Combine the polygons into a single sf object
  plot_sf <- do.call(rbind, plot_polygons)
  st_crs(plot_sf) <- sf::st_crs(plotPoints)
  plotID <- plotPoints$plotID
  
  plot_sf <- cbind(plot_sf, plotID)
  plot_sf <- plot_sf |> dplyr::left_join(plotPoints |> sf::st_drop_geometry()) 
  
  return(plot_sf)
  
}


# Function to create a full set of macrosystems plots
## Main operational function ----
generate.and.write.macro.plots.and.dem <- function(plotPoints, areaName) {
  
  # Setup directory
  exportLocation = here::here('data', 'fieldwork', 'plots', areaName)
  if(!dir.exists(exportLocation)) {
    dir.create(exportLocation)
  }
  
  #Generate landsat ARD raster template
  ardTemplateFile <- here::here(exportLocation, paste0(areaName, '_ARDTemplate.tif'))
  if(!file.exists(ardTemplateFile)) {
    landsatARDTemplate <- create.landsat.checkerboard(plotPoints)
    #write ard template raster
    terra::writeRaster(landsatARDTemplate,
                       ardTemplateFile,
                       overwrite = TRUE,
                       gdal=c("COMPRESS=DEFLATE"))
  } else {
    landsatARDTemplate <- terra::rast(ardTemplateFile)
  }
  
  
  #Get DEM for area
  areaDEMFile <- here::here(exportLocation, paste0(areaName, '_DEM.tif'))
  
  if(!file.exists(areaDEMFile)) {
    areaDEM <- elevatr::get_elev_raster(landsatARDTemplate, z = 12) |> #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#Key_information_about_version_0990_and_upcoming_versions_of_elevatr
      terra::rast() |> 
      terra::project(terra::crs('EPSG:4326'))
    
    #write DEM raster
    terra::writeRaster(areaDEM,
                       areaDEMFile,
                       overwrite = TRUE,
                       gdal=c("COMPRESS=DEFLATE"))
  } else {
    areaDEM <- terra::rast(areaDEMFile)
  }
  
  #Create plots that match with template
  plot_sf <- create.polygons.from.raster.and.points(raster = landsatARDTemplate, plotPoints)
  
  #Add functional plot buffer
  #This is area that can actually be used for data collection, and provides a guarantee of spatial overlap between the orthomosaics
  #and landsat pixels for testing unmixed product against classified imagery
  plot_sf <- plot_sf |> sf::st_buffer(10) #30 ft = 9.144 m. 
  
  # Print the final output
  print(plot_sf)
  
  
  #Export data
  
  #write plot packages
  sf::st_write(plot_sf, here::here(exportLocation, paste0(areaName, '.gpkg')), append = FALSE)
  sf::st_write(plot_sf, here::here(exportLocation, paste0(areaName, '.kml')), append = FALSE)
  
  #For each plot, export as .kml for UAS
  for(i in 1:nrow(plot_sf)) {
    
    #Write plot to shp
    plot <- plot_sf[i,]
    
    #Write plot to kml
    kmlnmP <- gsub(" ", "", paste(plot$plotID, ".kml", sep = ""))
    print(kmlnmP)
    
    #Write plot to kml for DJI use
    write.dji.kml(plot, kmlnmP, exportLocation)
    
    #Write plot to shapefile for use in metashape
    flNm <- gsub(" ", "", paste(plot$plotID, "_shp", sep = ""))
    st_write_shp(shp = plot,
                 location = exportLocation,
                 filename =  flNm,
                 zipOnly = FALSE,
                 overwrite = TRUE)
    #unlink(here::here(exportLocation, flNm, paste0(flNm, ".prj"))) #if need to remove .prj to make metashape accept it
    
    
    #Create pre-buffered plot for Map Pilot Pro
    plotBuffered <- plot |> sf::st_buffer(10) #30 ft = 9.144 m - this flight buffer is the ensure that the full area within the processed and clipped orthomosaic is usable and high quality
    kmlnmPBuffered <- gsub(" ", "", paste(plot$plotID, "_buffered.kml", sep = ""))
    sf::st_write(plotBuffered, here::here(exportLocation, kmlnmPBuffered), append = FALSE)
  }
  
}


# Create DJI-compatible KMLs


### ### ###
# A function to read in a KML file and turn it into a DJI-compatible KML, then export it
kml.to.dji.kml <- function(dir, kmlFile) {
  #Read KML file as raw txt
  kml <- readr::read_file(here::here(dir,kmlFile))
  
  #Get the coordinates from the original KML file
  coords <- kml %>% 
    stringr::str_match("<coordinates>\\s*(.*?)\\s*</coordinates>")
  coords <- coords[,2]
  
  #Add elevation to the original coordinates
  newCoords <- coords %>%
    gsub(" ", ",0 ", .) %>%
    paste0(., ",0", sep="")
  
  #Get name from original KML file
  nm <- kml %>%
    stringr::str_match("<name>\\s*(.*?)\\s*</name>")
  nm <- nm[,2]
  
  #Combine all text with the correctly formatted KML file text from a Google Earth Pro-created KML file
  newKMLtxt <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n<Document>\n\t<name>", nm, ".kml</name>\n\t<StyleMap id=\"m_ylw-pushpin\">\n\t\t<Pair>\n\t\t\t<key>normal</key>\n\t\t\t<styleUrl>#s_ylw-pushpin</styleUrl>\n\t\t</Pair>\n\t\t<Pair>\n\t\t\t<key>highlight</key>\n\t\t\t<styleUrl>#s_ylw-pushpin_hl</styleUrl>\n\t\t</Pair>\n\t</StyleMap>\n\t<Style id=\"s_ylw-pushpin\">\n\t\t<IconStyle>\n\t\t\t<scale>1.1</scale>\n\t\t\t<Icon>\n\t\t\t\t<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>\n\t\t\t</Icon>\n\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>\n\t\t</IconStyle>\n\t</Style>\n\t<Style id=\"s_ylw-pushpin_hl\">\n\t\t<IconStyle>\n\t\t\t<scale>1.3</scale>\n\t\t\t<Icon>\n\t\t\t\t<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>\n\t\t\t</Icon>\n\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>\n\t\t</IconStyle>\n\t</Style>\n\t<Placemark>\n\t\t<name>", nm, "</name>\n\t\t<styleUrl>#m_ylw-pushpin</styleUrl>\n\t\t<Polygon>\n\t\t\t<tessellate>1</tessellate>\n\t\t\t<outerBoundaryIs>\n\t\t\t\t<LinearRing>\n\t\t\t\t\t<coordinates>\n\t\t\t\t\t\t", newCoords, " \n\t\t\t\t\t</coordinates>\n\t\t\t\t</LinearRing>\n\t\t\t</outerBoundaryIs>\n\t\t</Polygon>\n\t</Placemark>\n</Document>\n</kml>\n", sep = "")
  
  #Sink to new kml file
  newFile <- paste(substr(kmlFile, 1, nchar(kmlFile)-4), "_for_dji.kml", sep="")
  sink(here::here(dir, newFile))
  cat(newKMLtxt)
  sink()
}

### ### ###
#The above, but as an EXPORT function
#This function takes in an sf object, exports it as a normal .kml, reads it back in, exports it as a dji.kml file, and deletes the original .kml file
#It is useful for keeping directories clean if you only need dji.kml files
#This function will download and load the 'sf' package if it is not already installed
#This function requires the function above (kml.to.dji.kml)
#sfObj is an sf object to write out, fileNm is the name of the file (e.g. mykml.kml), and outDir is the export directory (e.g.here::here('my', 'directory'))
write.dji.kml <- function(sfObj, fileNm, outDir) {
  #Check the required libraries and download if needed
  list.of.packages <- c("sf")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(sf)
  
  #Write as normal kml
  sf::st_write(sfObj, here(outDir, fileNm), append = FALSE)
  
  #Use kml.to.dji.kml function
  kml.to.dji.kml(outDir, fileNm)
  print(fileNm)
  
  #Remove original kml write-out
  if(file.exists(fileNm)) {
    unlink(here::here(outDir, fileNm))
  } else {
    print("File does not exist")
  }
}


# Creating half-diameter polygons ----

#' Calculate Approximate Diameter of Maximum Inscribed Circle
#'
#' This function calculates the approximate diameter of the maximum inscribed circle 
#' within irregular polygons in an `sf` object. The result is added as a new column 
#' named `diam` to the input `sf` object.
#'
#' @param polys An `sf` object representing the polygons.
#' @param tolerance A numeric value specifying the threshold for considering circles 
#' to be touching a boundary.
#'
#' @return An `sf` object containing the original polygons with an additional column `diam`, 
#' which represents the approximate diameter of the maximum inscribed circle for each polygon.
#'
#' @details
#' This function uses the `geos::geos_maximum_inscribed_crc()` function to calculate 
#' the largest inscribed circle for each polygon. The diameter is then estimated as 
#' the maximum distance between boundary points of the circle. The result is returned 
#' as an `sf` object with the `diam` column appended.
#'
#' @importFrom geos geos_maximum_inscribed_crc as_geos_geometry
#' @importFrom sf st_as_sf st_crs st_transform st_cast st_distance
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Example with sample polygons
#' library(sf)
#' library(geos)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Calculate diameters with a specified tolerance
#' nc_with_diam <- calculate_approx_diameter_maximum_inscribed_circle(nc, tolerance = 0.1)
#' head(nc_with_diam)
#' }
#'
#' @export
calculate_approx_diameter_maximum_inscribed_circle <- function(polys, tolerance) {
  max_insc_crcs <- geos::geos_maximum_inscribed_crc(polys |>
                                                      geos::as_geos_geometry(), tolerance = tolerance) |>
    sf::st_as_sf() |>
    sf::st_transform(sf::st_crs(polys))
  diam <- c()
  for (i in 1:nrow(max_insc_crcs)) {
    p <- max_insc_crcs[i,]
    d <- p |>
      sf::st_cast('MULTIPOINT') %>%
      sf::st_cast('POINT') %>%
      sf::st_distance(which = "Euclidean") |>
      max()
    diam <- diam |> append(d)
  }
  return(cbind(polys, diam))
}

#' Buffer Polygons to Half-Diameter
#'
#' This function buffers polygons inwards based on half the approximate diameter of 
#' their maximum inscribed circle. The resulting polygons are "half-diameter polygons".
#'
#' @param poly An `sf` object representing the polygons.
#' @param tolerance A numeric value specifying the threshold for considering circles 
#' to be touching a boundary.
#'
#' @return An `sf` object with the buffered polygons and an additional column 
#' `old_diam` that contains the original diameter values.
#'
#' @details
#' This function first calculates the approximate diameter of the maximum inscribed 
#' circle for each polygon using the 
#' \code{\link{calculate_approx_diameter_maximum_inscribed_circle}} function. It then 
#' buffers the polygons inward by a distance equal to half the diameter.
#'
#' The resulting polygons are added to a new column `geometry`, while the original 
#' diameters are stored in a column `old_diam`.
#'
#' @importFrom dplyr filter mutate rename
#' @importFrom sf st_buffer
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Example with sample polygons
#' library(sf)
#' library(geos)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Create half-diameter polygons
#' nc_half_diam <- buffer_to_half_diam(nc, tolerance = 0.1)
#' head(nc_half_diam)
#' }
#'
#' @export
buffer_to_half_diam <- function(poly, tolerance) {
  pWithD <- calculate_approx_diameter_maximum_inscribed_circle(poly, tolerance) |>
    dplyr::filter(is.finite(diam))
  newPolys <- pWithD |>
    dplyr::mutate(geometry = sf::st_buffer(geometry, dist = (diam / 4) * -1)) |>
    dplyr::rename(old_diam = diam)
  return(newPolys)
}



# Data access ----


## Hydrosheds DEM data ----

# A function to mount and download slope data for the US
# Returns the slope data object
# PARAMETERS
# slopeF :: the path to which the slope data should be saved (if not already present).
#If already downloaded, data will be read without download
access_us_slope <- function(slopeF) {
  if(file.exists(slopeF)) {
    slope <- terra::rast(slopeF)
  } else {
    slope <- glue::glue(
      "/vsizip/vsicurl/", #magic remote connection 
      "https://data.hydrosheds.org/file/hydrosheds-v1-dem/hyd_na_dem_15s.zip", #copied link to download location
      "/hyd_na_dem_15s.tif") |> #path inside zip file
      terra::rast() |>
      terra::terrain("slope")
    terra::writeRaster(slope, slopeF,
                       gdal=c("COMPRESS=DEFLATE"))
  }
  return(slope)
}

## OSM Road Data ----


#A function to access road data from OSM
# PARAMETERS
# aoi :: an area of interest as an sf object - roads will be accessed within the area plus a 1km buffer
# Adapt function as necessary for filtering
access_osm_roads <- function(aoi) {
  roadsData <- osmdata::opq(bbox = sf::st_bbox(sf::st_transform(sf::st_buffer(aoi, 2000), 4326))) |>
    osmdata::add_osm_feature(key = "highway",
                             key_exact = FALSE,
                             value_exact = FALSE,
                             match_case = FALSE) |>
    osmdata::osmdata_sf()
  desiredColumns <- c("USFS", "highway", "access", "maintained", "motor_vehicle", "service", "smoothness", "surface", "tracktype")
  roads <- roadsData$osm_lines |>
    dplyr::select(dplyr::any_of(desiredColumns)) |>
    dplyr::filter(highway != "path" | is.na(highway)) |> 
    dplyr::filter(tracktype != "grade5" | is.na(tracktype)) |>
    dplyr::filter(access != "private" | is.na(access)) |>
    dplyr::mutate(group = 1) |>
    group_by(group) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup() |>
    sf::st_transform(epsg) |>
    sf::st_intersection(sf::st_buffer(aoi, 1000)) #clip to district of interest + 1km
  
  return(roads)
}


## Administrative boundaries ----




access_ynp_bear_management_areas <- function() {
  # Write out the URL query
  base_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/YELL_BEAR_MANAGEMENT_AREAS_public_viewview/FeatureServer/0/query"
  query_params <- list(f = "json",
                       where = "1=1",
                       outFields = "*",
                       returnGeometry = "true")
  
  # Request data
  bma <- access_data_get_x_from_arcgis_rest_api_geojson(
    base_url = base_url, 
    query_params = query_params, 
    max_record = 1000, 
    n = "all", 
    timeout = 600
  )
  
  return(bma)
  
}

# The ranger districts file is quite small, so it is accessed via VSI
access_usfs_ranger_districts <- function() {
  usfs_rds <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.RangerDistrict.zip", #copied link to download location
    "/S_USA.RangerDistrict.shp") |> #path inside zip file
    sf::st_read()
  return(usfs_rds)
}


# A function to access the US federal surface management agency polygon dataset
# The function downloads and unzips a geodatabase rather than accessing via VSI since this layer is 
# useful for visualization and field planning, as well as it being accessed multiple times
access_us_sma <- function(dir_path, layer) {
  
  loc <- here::here(dir_path, "SMA_WM.gdb")
  if(file.exists(loc)) {
    sma <- sf::st_read(loc, layer = layer)
  } else {
    download_unzip_file(url = "https://blm-egis.maps.arcgis.com/sharing/rest/content/items/6bf2e737c59d4111be92420ee5ab0b46/data",
                        extract_to = dir_path,
                        keep_zip = FALSE)
    sma <-  sf::st_read(loc, layer = layer)
  }
  return(sma)
}

access_us_sma_helper_show_layers <- function(dir_path) {
 sf::st_layers(here::here(dir_path, "SMA_WM.gdb"))
}

access_us_wilderness <- function(dest_path = NA) {
  if(is.na(dest_path)) {
    wild <- paste0(
      "/vsizip/vsicurl/", #magic remote connection
      "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip", #copied link to download location
      "/S_USA.Wilderness.shp") |> #path inside zip file
      sf::st_read()
  } else {
    if(file.exists(dest_path)) {
      wild <- sf::st_read(dest_path)
    } else {
      wild <- paste0(
        "/vsizip/vsicurl/", #magic remote connection
        "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.Wilderness.zip", #copied link to download location
        "/S_USA.Wilderness.shp") |> #path inside zip file
        sf::st_read()
      sf::st_write(wild, dest_path)
    }
  }
  return(wild)
}


# Queries the PADUS REST service for WSAs
# PADUS interactive online: https://usgs.maps.arcgis.com/home/item.html?id=98fce3fb0c8241ce8847e9f7d0d212e9
access_us_wilderness_study_areas <- function(dest_path = NA) {
  if(is.na(dest_path)) {
    query_params <- list(where = "DesTp_Desc='Wilderness Study Area'",
                         outFields = "*",
                         f = "json")
    base_url = "https://services.arcgis.com/v01gqwM5QqNysAAi/ArcGIS/rest/services/PADUS_Protection_Status_by_GAP_Status_Code/FeatureServer/0/QUERY"
    wsa <- access_data_get_x_from_arcgis_rest_api_geojson(base_url = base_url,
                                                          query_params = query_params,
                                                          max_record = 2000,
                                                          n = "all",
                                                          timeout = 500)
  } else {
    if(file.exists(dest_path)) {
      wsa <- sf::st_read(dest_path)
    } else {
      query_params <- list(where = "DesTp_Desc='Wilderness Study Area'",
                           outFields = "*",
                           f = "json")
      base_url = "https://services.arcgis.com/v01gqwM5QqNysAAi/ArcGIS/rest/services/PADUS_Protection_Status_by_GAP_Status_Code/FeatureServer/0/QUERY"
      wsa <- access_data_get_x_from_arcgis_rest_api_geojson(base_url = base_url,
                                                            query_params = query_params,
                                                            max_record = 2000,
                                                            n = "all",
                                                            timeout = 500)
      sf::st_write(wsa, dest_path)
    }
  }
  return(wsa)
}


## Landfire ----

#' Access LANDFIRE EVT Raster for CONUS (2023)
#'
#' This function remotely accesses and reads the 2023 LANDFIRE Existing Vegetation Type (EVT) raster data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' The function utilizes GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT raster file without needing to download or unzip it manually. The raster data is read into a `terra` raster object, suitable for geospatial analysis in R.
#'
#' @return
#' A `terra` raster object containing the 2023 LANDFIRE EVT data for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt <- access_landfire_evt_conus_2023()
#' plot(lf_evt)
#' }
#' 
#' @importFrom terra rast
#' @export
access_landfire_evt_conus_2023 <- function() {
  lf_evt <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_240/LF2023_EVT_240_CONUS.zip", #copied link to download location
    "/LF2023_EVT_240_CONUS/Tif/LC23_EVT_240.tif") |> #path inside zip file
    terra::rast()
  return(lf_evt)
}

#' Access LANDFIRE EVT CSV for CONUS (2023)
#'
#' This function remotely accesses and reads the 2023 LANDFIRE Existing Vegetation Type (EVT) CSV data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' This function uses GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT CSV data without manual download or extraction. The CSV is read into an `sf` object using `sf::st_read()`, as GDAL's CSV handling is supported by spatial data functions. This method is necessary since standard R CSV readers do not natively support remote access via VSI.
#'
#' @return
#' An `sf` object containing the CSV data from the 2023 LANDFIRE EVT for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt_csv <- access_landfire_evt_conus_2023_csv()
#' head(lf_evt_csv)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_landfire_evt_conus_2023_csv <- function() {
  lf_evt_csv <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_240/LF2023_EVT_240_CONUS.zip", #copied link to download location
    "/LF2023_EVT_240_CONUS/CSV_Data/LF23_EVT_240.csv") |> #path inside zip file
    sf::st_read() #note that read_csv and other csv drivers in R don't talk to GDAL. Instead use st_read or terra::vect() to access CSV data in zip files
  return(lf_evt_csv)
}

#' Access LANDFIRE EVT Raster for CONUS (2022)
#'
#' This function remotely accesses and reads the 2022 LANDFIRE Existing Vegetation Type (EVT) raster data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' The function utilizes GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT raster file without needing to download or unzip it manually. The raster data is read into a `terra` raster object, suitable for geospatial analysis in R.
#'
#' @return
#' A `terra` raster object containing the 2022 LANDFIRE EVT data for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt <- access_landfire_evt_conus_2022()
#' plot(lf_evt)
#' }
#' @param access Either "stream" or "download". "stream" will use VSI to access the dataset remotely.
#' "download" will check if the data is present within the directory given at dir_path, and, if not there, will download it before accessing it.
#'
#' @importFrom terra rast
#' @export
access_landfire_evt_conus_2022 <- function(access = "stream", dir_path = NA) {
  
  if(access == "stream") {
    lf_evt <- paste0(
      "/vsizip/vsicurl/", #magic remote connection
      "https://landfire.gov/data-downloads/US_230/LF2022_EVT_230_CONUS.zip", #copied link to download location
      "/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif") |> #path inside zip file
      terra::rast()
  }
  if(access == "download") {
    loc <- here::here(dir_path, "LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif")
    if(file.exists(loc)) {
      lf_evt <- terra::rast(loc)
    } else {
      download_unzip_file(url = "https://landfire.gov/data-downloads/US_230/LF2022_EVT_230_CONUS.zip",
                          extract_to = dir_path,
                          keep_zip = FALSE)
      lf_evt <- terra::rast(loc)
    }
  }

  return(lf_evt)
}

#' Access LANDFIRE EVT CSV for CONUS (2022)
#'
#' This function remotely accesses and reads the 2022 LANDFIRE Existing Vegetation Type (EVT) CSV data for the contiguous United States (CONUS). The data is accessed directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#'
#' @details
#' This function uses GDAL's virtual file system (`/vsizip/vsicurl/`) to remotely access the LANDFIRE EVT CSV data without manual download or extraction. The CSV is read into an `sf` object using `sf::st_read()`, as GDAL's CSV handling is supported by spatial data functions. This method is necessary since standard R CSV readers do not natively support remote access via VSI.
#'
#' @return
#' An `sf` object containing the CSV data from the 2022 LANDFIRE EVT for CONUS.
#'
#' @examples
#' \dontrun{
#' lf_evt_csv <- access_landfire_evt_conus_2022_csv()
#' head(lf_evt_csv)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_landfire_evt_conus_2022_csv <- function() {
  lf_evt_csv <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_230/LF2022_EVT_230_CONUS.zip", #copied link to download location
    "/LF2022_EVT_230_CONUS/CSV_Data/LF22_EVT_230.csv") |> #path inside zip file
    sf::st_read() #note that read_csv and other csv drivers in R don't talk to GDAL. Instead use st_read or terra::vect() to access CSV data in zip files
  return(lf_evt_csv)
}

## NEON ----

#' Access AOP Flight Box Data
#'
#' Retrieves the flight box shapefile data for all NEON AOP sites by downloading and reading
#' it from the specified remote location.
#'
#' @return An sf object containing the flight box data for all NEON AOP sites.
#'
#' @importFrom sf st_read
#' 
#' @export
access_neon_aop_flight_box_data <- function() {
  aop_all <- paste0(
    "/vsizip/vsicurl/", # Magic remote connection
    "https://www.neonscience.org/sites/default/files/AOP_flightBoxes_0.zip", # Copied link to download location
    "/AOP_flightBoxes/AOP_flightboxesAllSites.shp") |> # Path inside zip file
    sf::st_read()
  return(aop_all)
}

#' Access NEON Plot Shapefiles
#'
#' This function remotely accesses and reads the NEON plot shapefiles directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#' It downloads the shapefile for all NEON TOS (Tower Observation System) plots.
#'
#' @details
#' This function uses a magic remote connection through GDAL's virtual file system (`/vsizip/vsicurl/`) to access the shapefile directly from the zipped NEON data repository. The function does not require the user to download or unzip the file manually. The shapefile is read into an `sf` object for further spatial analysis in R.
#'
#' @return
#' An `sf` object containing the NEON TOS plot polygons.
#'
#' @examples
#' \dontrun{
#' neon_plots <- access_neon_plots_shp()
#' plot(neon_plots)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_neon_plots_shp <- function() {
  neon_plots <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V10.zip", #copied link to download location
    "/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Polygons_V10.shp") |> #path inside zip file
    sf::st_read()
  return(neon_plots)
}

#' Access NEON Domain Shapefiles
#'
#' This function remotely accesses and reads the NEON domain shapefiles directly from a zipped online source using GDAL's VSI (Virtual File System) protocol.
#' It downloads the shapefile for NEON's defined geographic domains.
#'
#' @details
#' Similar to the `access_neon_plots_shp()` function, this function uses the GDAL virtual file system (`/vsizip/vsicurl/`) to access and read NEON domain shapefiles directly from a zipped source. The shapefile contains geographic domain boundaries for the NEON project, which is read into an `sf` object.
#'
#' @return
#' An `sf` object containing the NEON domain polygons.
#'
#' @examples
#' \dontrun{
#' neon_domains <- access_neon_domains_shp()
#' plot(neon_domains)
#' }
#' 
#' @importFrom sf st_read
#' @export
access_neon_domains_shp <- function() {
  neon_domains <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://www.neonscience.org/sites/default/files/NEONDomains_0.zip", #copied link to download location
    "/NEON_Domains.shp") |> #path inside zip file
    sf::st_read()
  return(neon_domains)
}



## Ecoregions ----



#' Access EPA Level II Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level II ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level II ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level II ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l2_ecoregions_vsi()
#'
access_data_epa_l2_ecoregions_vsi <- function() {
  epa_l2 <- paste0(
    "/vsizip/vsicurl/",
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip",
    "/NA_CEC_Eco_Level2.shp"
  ) |>
    sf::st_read()
  
  return(epa_l2)
}

#' Access EPA Level III Ecoregions Data via VSI
#'
#' This function retrieves the U.S. EPA Level III ecoregions shapefile from a remote server via VSI (Virtual Spatial Infrastructure).
#' The shapefile is stored in a ZIP file, and the function accesses it without downloading the file locally.
#'
#' @return A `sf` (simple features) object containing the EPA Level III ecoregions shapefile data.
#' 
#' @details
#' The function accesses the EPA Level III ecoregions shapefile directly from the EPA's data commons, utilizing the `/vsizip/` 
#' and `/vsicurl/` mechanisms to stream the shapefile from the zipped file. The file is accessed via a URL without the need to 
#' download it locally. This method allows efficient access to the shapefile data using the `sf` package.
#'
#' @references
#' U.S. EPA Ecoregions Information: \url{https://www.epa.gov/eco-research/ecoregions-north-america}
#'
#' @importFrom sf st_read
#' @export
#' @examples
#' # Example usage
#' epa_ecoregions <- access_data_epa_l3_ecoregions_vsi()
#'
access_data_epa_l3_ecoregions_vsi <- function() {
  epa_l3 <- paste0(
    "/vsizip/vsicurl/",
    "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/cec_na/NA_CEC_Eco_Level3.zip",
    "/NA_CEC_Eco_Level3.shp"
  ) |>
    sf::st_read()
  
  return(epa_l3)
}

## ArcGIS Online ----

#' Get an ArcGIS Online Token
#'
#' This function generates a token for accessing ArcGIS Online resources. 
#' It prompts the user for their ArcGIS username and password and uses these 
#' credentials to obtain a token via the ArcGIS REST API.
#'
#' @return A character string containing the ArcGIS Online token.
#'
#' @details 
#' The function uses the ArcGIS REST API endpoint 
#' (\url{https://www.arcgis.com/sharing/rest/generateToken}) to authenticate the 
#' user and generate a token. The token can be used for subsequent API requests 
#' to access ArcGIS Online resources. The function makes use of the `httr` 
#' package for the HTTP POST request and the `askpass` package to securely 
#' request the user's password.
#'
#' @importFrom httr POST content
#' @importFrom askpass askpass
#'
#' @examples
#' \dontrun{
#' # Generate an ArcGIS Online token
#' token <- get_arcgis_online_token()
#' print(token)
#' }
#'
#' @export
get_arcgis_online_token <- function() {
  username <- readline(prompt = "Enter your ArcGIS username: ")
  password <- askpass::askpass(prompt = "Enter your ArcGIS password: ")
  
  response <- httr::POST(
    url = "https://www.arcgis.com/sharing/rest/generateToken",
    body = list(
      username = username,
      password = password,
      referer = "https://www.arcgis.com",
      f = "json"
    )
  )
  
  token <- httr::content(response)$token
  return(token)
}




# Utility ----


install_and_load_packages <- function(package_list) {
  if (!is.character(package_list)) {
    stop("package_list must be a character vector.")
  }
  
  # Ensure 'pak' is installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    message("The 'pak' package is required for fast installation of packages, installing now.")
    install.packages("pak")
  }
  
  # Split into CRAN and GitHub packages
  cran_packages <- package_list[!grepl("/", package_list)]
  github_packages <- package_list[grepl("/", package_list)]
  
  # Identify missing packages
  missing_cran <- cran_packages[!sapply(cran_packages, requireNamespace, quietly = TRUE)]
  missing_github <- github_packages[!sapply(gsub(".*/", "", github_packages), requireNamespace, quietly = TRUE)]
  
  # Install missing CRAN packages one by one
  for (pkg in missing_cran) {
    message("Installing missing CRAN package: ", pkg)
    tryCatch({
      pak::pkg_install(pkg, upgrade = TRUE)
    }, error = function(e) {
      message("❌ Failed to install ", pkg, ": ", e$message)
    })
  }
  
  # Install missing GitHub packages one by one
  for (pkg in missing_github) {
    message("Installing missing GitHub package: ", pkg)
    tryCatch({
      pak::pkg_install(pkg, upgrade = TRUE)
    }, error = function(e) {
      message("❌ Failed to install ", pkg, ": ", e$message)
    })
  }
  
  # Re-check if packages installed successfully
  still_missing <- package_list[!sapply(gsub(".*/", "", package_list), requireNamespace, quietly = TRUE)]
  
  if (length(still_missing) > 0) {
    message("⚠️ The following packages failed to install: ", paste(still_missing, collapse = ", "))
    message("Try manually running: pak::pkg_install(c(", paste(shQuote(still_missing), collapse = ", "), "))")
    return(invisible())
  }
  
  # Load installed packages
  for (pkg in cran_packages) {
    if (!library(pkg, character.only = TRUE, logical.return = TRUE)) {
      message("Failed to load CRAN package: ", pkg)
    }
  }
  
  for (pkg in github_packages) {
    pkg_name <- gsub(".*/", "", pkg)
    if (!library(pkg_name, character.only = TRUE, logical.return = TRUE)) {
      message("Failed to load GitHub package: ", pkg_name)
    }
  }
  
  message("✅ All specified packages installed and loaded.")
}


#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' #' If any packages are missing, it installs them automatically without asking for user confirmation.
#' #' It uses the `pak` package for faster and more efficient package installation.
#' #'
#' #' @param package_list A character vector of package names (e.g., `c("dplyr", "here")`).
#' #' GitHub packages should be specified as `username/repo` in strings.
#' #' @return No return value. Installs and loads the specified packages as needed.
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c("dplyr", "here", "username/repo"))
#' #' }
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list) {
#'   if (!is.character(package_list)) {
#'     stop("package_list must be a character vector.")
#'   }
#'   
#'   # Ensure 'pak' is installed
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     message("The 'pak' package is required for fast installation of packages, installing now.")
#'     install.packages("pak")
#'   }
#'   
#'   # Split into CRAN and GitHub packages
#'   cran_packages <- package_list[!grepl("/", package_list)]
#'   github_packages <- package_list[grepl("/", package_list)]
#'   
#'   # Identify missing packages
#'   missing_cran <- cran_packages[!sapply(cran_packages, requireNamespace, quietly = TRUE)]
#'   missing_github <- github_packages[!sapply(gsub(".*/", "", github_packages), requireNamespace, quietly = TRUE)]
#'   
#'   # Install missing CRAN packages
#'   if (length(missing_cran) > 0) {
#'     message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
#'     tryCatch({
#'       pak::pkg_install(missing_cran, upgrade = TRUE)
#'     }, error = function(e) {
#'       message("Failed to install some CRAN packages: ", e$message)
#'     })
#'   }
#'   
#'   # Install missing GitHub packages
#'   if (length(missing_github) > 0) {
#'     message("Installing missing GitHub packages: ", paste(missing_github, collapse = ", "))
#'     tryCatch({
#'       pak::pkg_install(missing_github, upgrade = TRUE)
#'     }, error = function(e) {
#'       message("Failed to install some GitHub packages: ", e$message)
#'     })
#'   }
#'   
#'   # Load installed packages
#'   for (pkg in cran_packages) {
#'     if (!library(pkg, character.only = TRUE, logical.return = TRUE)) {
#'       message("Failed to load CRAN package: ", pkg)
#'     }
#'   }
#'   
#'   for (pkg in github_packages) {
#'     pkg_name <- gsub(".*/", "", pkg)
#'     if (!library(pkg_name, character.only = TRUE, logical.return = TRUE)) {
#'       message("Failed to load GitHub package: ", pkg_name)
#'     }
#'   }
#'   
#'   message("All specified packages installed and loaded.")
#' }





#' 
#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' #' If any packages are missing, it installs them automatically.
#' #' It uses the `pak` package for faster and more efficient package installation.
#' #'
#' #' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' #' GitHub packages should be specified as `username/repo` in strings.
#' #' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' #' without asking for user permission. If "n", asks for permission from the user.
#' #' @return No return value. Installs and loads the specified packages as needed.
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c(dplyr, here, "username/repo"))
#' #' }
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list, auto_install = "n") {
#'   # Convert non-string package names to strings
#'   package_list <- lapply(package_list, function(pkg) {
#'     if (is.symbol(pkg)) {
#'       deparse(substitute(pkg))
#'     } else {
#'       pkg
#'     }
#'   })
#'   
#'   # # Check if 'renv' is installed; if not, skip the 'renv' check
#'   # if (requireNamespace("renv", quietly = TRUE) && renv::is_active()) {
#'   #   cat("renv is active. Only loading packages...\n")
#'   #   for (pkg in package_list) {
#'   #     package_name <- if (grepl("/", pkg)) unlist(strsplit(pkg, "/"))[2] else pkg
#'   #     if (!require(package_name, character.only = TRUE)) {
#'   #       cat("Failed to load package:", package_name, "\n")
#'   #     }
#'   #   }
#'   #   return(invisible())
#'   # }
#'   
#'   # Check if pak is installed; install if not
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Initialize lists to store missing CRAN and GitHub packages
#'   missing_cran_packages <- c()
#'   missing_github_packages <- c()
#'   
#'   # # Helper function to get user input
#'   # get_user_permission <- function(prompt_msg) {
#'   #   if (auto_install == "y") {
#'   #     return("y")
#'   #   } else {
#'   #     return(tolower(readline(prompt = prompt_msg)))
#'   #   }
#'   # }
#'   
#'   # Check for missing packages
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
#'     } else { # CRAN package
#'       package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
#'     }
#'     if (!package_loaded) {
#'       if (grepl("/", pkg)) {
#'         missing_github_packages <- c(missing_github_packages, pkg)
#'       } else {
#'         missing_cran_packages <- c(missing_cran_packages, pkg)
#'       }
#'     }
#'   }
#'   
#'   # Install missing CRAN packages using pak::pkg_install
#'   if (length(missing_cran_packages) > 0) {
#'     # cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
#'     # if (response == "y") {
#'     pak::pkg_install(missing_cran_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing CRAN packages.\n")
#'     # }
#'   }
#'   
#'   # Install missing GitHub packages using pak::pkg_install
#'   if (length(missing_github_packages) > 0) {
#'     # cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
#'     # if (response == "y") {
#'     pak::pkg_install(missing_github_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing GitHub packages.\n")
#'     # }
#'   }
#'   
#'   # Load all packages after checking for installation
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       if (!require(package_name, character.only = TRUE)) {
#'         cat("Failed to load GitHub package:", package_name, "\n")
#'       }
#'     } else { # CRAN package
#'       if (!require(pkg, character.only = TRUE)) {
#'         cat("Failed to load CRAN package:", pkg, "\n")
#'       }
#'     }
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }


#' Ensure Directory Exists
#'
#' This function checks if a directory exists at the specified path, and if not, creates a new directory.
#'
#' @param path A character string specifying the path to the new directory.
#' @return The function does not return any value. It creates a directory if it does not already exist.
#' @examples
#' # Ensure a directory named "data" exists
#' dir_ensure("data")
#'
#' @export
dir_ensure <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    message("Directory created: ", path)
  } else {
    message("Directory already exists: ", path)
  }
}


# Function Description:
#   The function create.qgis.style.for.paletted.raster.from.csv generates a QGIS style file (.qml) for raster layers, specifically formatted for paletted rasters. It uses styling information provided in a data frame (styleData), allowing users to define colors and labels for different raster values. The function supports hexadecimal (hex) and RGB color schemes.
# 
# Parameters:
#   styleData: A data frame containing the styling information. The data frame should include the columns specified by valueColumn and labelColumn. For the hex color scheme, a color column is required. For RGB, columns R, G, and B are necessary.
# outputQmlPath: A string specifying the file path where the generated QGIS style file (.qml) will be saved.
# valueColumn: The name of the column in styleData that contains the raster values.
# labelColumn: The name of the column in styleData that contains the labels for each raster value.
# colorScheme: A string indicating the color scheme used in styleData. It can be "hex" for hexadecimal colors or "RGB" for separate red, green, and blue values. The default is "hex".
# Functionality:
#   The function iterates through each row of styleData, extracting the value, label, and color information to create palette entries in the QML file. For RGB color schemes, it converts the RGB values to hex using the rgb function. The function ensures proper XML formatting by escaping special characters in labels. After constructing the QML content, it is written to the specified output path.
# 
# Usage Example:
# # Assuming styleData is pre-defined with the appropriate columns
# create_qgis_style_for_paletted_raster_from_csv(styleData, "path/to/output.qml", "value", "label", "hex")
# Citation:
#   Function authored by R Code Stylist, GPT-4, OpenAI, in collaboration with the user.
create_qgis_style_for_paletted_raster_from_csv <- function(styleData, outputQmlPath, valueColumn, labelColumn, colorScheme = "hex") {
  
  # Check for the necessary columns in the CSV based on the color scheme
  if (!labelColumn %in% colnames(styleData)) {
    stop("CSV file must contain the specified label column.")
  }
  
  if (colorScheme == "hex" && !("color" %in% colnames(styleData))) {
    stop("CSV file must contain a 'color' column for hex color scheme.")
  } else if (colorScheme == "RGB" && !all(c("R", "G", "B") %in% colnames(styleData))) {
    stop("CSV file must contain 'R', 'G', 'B' columns for RGB color scheme.")
  }
  
  # Start creating the QML content
  qmlContent <- '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE qgis PUBLIC \'http://mrcc.com/qgis.dtd\' \'SYSTEM\'>
<qgis hasScaleBasedVisibilityFlag="0" maxScale="0" version="3.22.12-Białowieża" styleCategories="AllStyleCategories" minScale="1e+08">
   <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal enabled="0" fetchMode="0" mode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option value="false" type="bool" name="WMSBackgroundLayer"/>
      <Option value="false" type="bool" name="WMSPublishDataSourceUrl"/>
      <Option value="0" type="int" name="embeddedWidgets/count"/>
      <Option value="Value" type="QString" name="identify/format"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer opacity="1" nodataColor="" type="paletted" band="1" alphaBand="-1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
  <colorPalette>'  
  # Append palette entries from the CSV data
  for (i in 1:nrow(styleData)) {
    # Determine the color based on the scheme
    if (colorScheme == "hex") {
      color <- styleData$color[i]
    } else {
      color <- rgb(red = styleData$R[i], green = styleData$G[i], blue = styleData$B[i], maxColorValue = 255)
    }
    
    label <- styleData[[labelColumn]][i]
    label <- gsub("&", "and", label)
    label <- gsub('\\"', '', label)
    value <- styleData[[valueColumn]][i]
    
    
    qmlContent <- glue::glue('{qmlContent}
             <paletteEntry value="{value}" label="{label}" alpha="255" color="{color}"/>'
    )
  }
  
  # Finalize the QML content with closing tags
  qmlContent <- paste0(qmlContent, '\n      </colorPalette>
          <colorramp type="randomcolors" name="[source]">
        <Option/>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast gamma="1" brightness="0" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeOn="0" colorizeGreen="128" saturation="0" colorizeBlue="128" colorizeRed="255" colorizeStrength="100" invertColors="0"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>')
  
  # Write the QML content to a file
  writeLines(qmlContent, outputQmlPath)
  
  print(qmlContent)
  
  return(paste("QGIS style file created at:", outputQmlPath))
}

#' Write Shapefile to a New Directory and Create a Zipped Version
#'
#' This function writes an `sf` object to a shapefile in a new, file-specific directory and optionally creates a zipped version of the shapefile.
#' It also allows for the removal of the original unzipped files and handles overwriting existing files.
#'
#' @param shp An `sf` object to write as a shapefile.
#' @param location A character string specifying the path of the directory to create the new file-specific subdirectory in.
#' @param filename A character string specifying the name of the file without the `.shp` extension.
#' @param zip_only A logical value indicating whether the original (unzipped) files should be removed after zipping. Defaults to `FALSE`.
#' @param overwrite A logical value indicating whether existing files should be overwritten. Defaults to `FALSE`.
#' @return No return value. The function writes a shapefile to a specified directory, optionally zips the files, and manages file cleanup based on user input.
#' @examples
#' \dontrun{
#' # Example usage
#' st_write_shp(shp = prepped_for_parks_etal,
#'              location = here::here("data/derived"),
#'              filename = "career_lba_for_parks_v1",
#'              zip_only = TRUE,
#'              overwrite = TRUE)
#' }
#' @importFrom sf st_write
#' @importFrom zip zip
#' @export
st_write_shp <- function(shp, location, filename, zip_only = FALSE, overwrite = FALSE) {
  
  # Define paths
  out_dir <- file.path(location, filename)
  zip_file <- file.path(out_dir, paste0(filename, ".zip"))
  zip_file_dest <- file.path(location, paste0(filename, ".zip"))
  
  # Manage overwriting and directory creation
  if (dir.exists(out_dir)) {
    if (overwrite) {
      unlink(out_dir, recursive = TRUE)
    } else {
      stop("Directory '", out_dir, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  if (file.exists(zip_file_dest) && zip_only) {
    if (overwrite) {
      unlink(zip_file_dest)
    } else {
      stop("Zip file '", zip_file_dest, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  # Create the directory if not there
  dir_ensure(out_dir)
  
  # Write the shapefile
  shapefile_path <- file.path(out_dir, paste0(filename, ".shp"))
  sf::st_write(shp, shapefile_path, append = FALSE)
  
  # Get all shapefile components
  all_shp_files <- list.files(out_dir, pattern = paste0(filename, ".*"), full.names = TRUE)
  
  # Create zip file
  zip::zip(zipfile = zip_file, files = all_shp_files, mode = "cherry-pick")
  
  # Remove raw files if zip_only is TRUE
  if (zip_only) {
    file.copy(zip_file, zip_file_dest)
    unlink(out_dir, recursive = TRUE)
  }
}


#' Add Polygon Areas to an sf Object
#'
#' This function calculates the area of each polygon in an `sf` object, 
#' appends the area as a new column, and names the column based on the user-specified parameter.
#'
#' @param polys An `sf` object containing polygon geometries.
#' @param nm A character string specifying the name of the new column to store polygon areas.
#'
#' @return An `sf` object with an additional column containing the polygon areas (in the same units as the input `sf` object).
#'
#' @details 
#' The function computes polygon areas using `sf::st_area()`, removes the units with `units::drop_units()`, 
#' and appends the areas as a new column to the input `sf` object. The new column's name is defined by the user 
#' through the `nm` argument. The function maintains the structure and properties of the original `sf` object.
#'
#' @importFrom sf st_area
#' @importFrom units drop_units
#' @importFrom dplyr rename
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Load example data
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#'
#' # Add a column with polygon areas
#' nc_with_area <- st_area_to_poly(nc, "area")
#' head(nc_with_area)
#' }
#'
#' @export
st_area_to_poly <- function(polys, nm) {
  out <- polys |>
    sf::st_area() |> # get area from sf package
    units::drop_units() %>%
    cbind(polys, .) |> # join to polygons
    dplyr::rename({{nm}} := `.`) # rename
  return(out)
}



#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped to vector
#Returns raster in the same form that it came in
# PARAMETERS
# raster : a SpatRaster, PackedSpatRaster, RasterLayer, RasterStack, or RasterBrick object
# vector : a SpatVector, PackedSpatVector or SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
crop_careful_universal <- function(raster, vector, mask, verbose = FALSE) {
  pack <- FALSE
  
  #Unpack if parallelized inputs
  if(class(raster)[1] == "PackedSpatRaster") {
    raster <- terra::unwrap(raster)
    pack <- TRUE
  }
  if(class(vector)[1] == "PackedSpatVector") {
    vector <- sf::st_as_sf(terra::unwrap(vector))
  }
  
  #Handle unpacked spatVector
  if(class(vector)[1] == "SpatVector") {
    vector <- sf::st_as_sf(vector)
  }
  
  #If using raster package
  if(class(raster)[1] == "RasterLayer" | class(raster)[1] == "RasterStack" | class(raster)[1] == "RasterBrick") {
    
    #Perform operation
    if (raster::crs(vector) != raster::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
      if(verbose) {print("Projecting vector")}
      vector <- sf::st_transform(vector, raster::crs(raster)) 
    } else {
      if(verbose) {print("Vector already in raster CRS")}
    }
    if(verbose) {print("Clipping")}
    r <- raster::crop(raster,
                      vector)
    if(mask) {
      r <- r |> raster::mask(vector)
    }
    
    return(r)
    
  } else { #terra package
    
    #Perform operation
    if (terra::crs(vector) != terra::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
      if(verbose) {print("Projecting vector")}
      vector <- sf::st_transform(vector, terra::crs(raster)) 
    } else {
      if(verbose) {print("Vector already in raster CRS")}
    }
    if(verbose) {print("Clipping")}
    r <- terra::crop(raster,
                     vector,
                     mask = mask) #crop & mask
    
    #Repack if was packed coming in (i.e. parallelized)
    if(pack) {
      r <- terra::wrap(r)
    }
    return(r)
    
  }
}

#' Substring from the Right
#'
#' This function extracts a substring from the right side of a given string, retaining a specified number of characters.
#' It will work for either single strings or a vectorized input (e.g. when used on a data frame)
#'
#' @param str A character string from which to extract the substring.
#' @param n An integer specifying the number of characters to keep from the right of the string.
#' @return A character string containing the rightmost \code{n} characters of the input string.
#' @examples
#' # Extract the last 3 characters from a string
#' substr_right("Hello, World!", 3)
#'
#' @export
substr_right <- function(str, n) {
  # Check if input is a vector, and apply the function element-wise using sapply
  if (length(str) > 1) {
    return(sapply(str, function(x) {
      if (n > nchar(x)) {
        warning("n is greater than the length of the string. Returning the full string.")
        return(x)
      }
      return(substr(x, nchar(x) - n + 1, nchar(x)))
    }))
  }
  
  # If input is a single string, apply the logic directly
  if (n > nchar(str)) {
    warning("n is greater than the length of the string. Returning the full string.")
    return(str)
  }
  
  return(substr(str, nchar(str) - n + 1, nchar(str)))
}

#' Download and Unzip a File
#'
#' Downloads a ZIP file from a specified URL and extracts its contents to a specified directory.
#' Optionally, the ZIP file can be retained after extraction.
#'
#' @param url Character. The URL of the ZIP file to download.
#' @param extract_to Character. The directory where the contents should be extracted.
#' @param keep_zip Logical. If `TRUE`, retains the ZIP file after extraction. Defaults to `FALSE`.
#'
#' @return Invisible `NULL`. The function is used for its side effects of downloading and extracting files.
#'
#' @details The function downloads a ZIP file from a URL and extracts its contents to a specified directory.
#' If `keep_zip` is set to `FALSE`, the ZIP file will be deleted after extraction.
#'
#' @importFrom utils download.file unzip
#' @export
#'
#' @examples
#' \dontrun{
#' download_unzip_file("https://example.com/data.zip", "path/to/extract", keep_zip = TRUE)
#' }
download_unzip_file <- function(url, extract_to, keep_zip = FALSE) {
  # Validate URL and extraction path
  if (!is.character(url) || length(url) != 1) stop("`url` must be a single character string.")
  if (!is.character(extract_to) || length(extract_to) != 1) stop("`extract_to` must be a single character string.")
  if (!is.logical(keep_zip) || length(keep_zip) != 1) stop("`keep_zip` must be a single logical value.")
  
  # Ensure the extraction directory exists
  if (!dir.exists(extract_to)) dir.create(extract_to, recursive = TRUE)
  
  # Determine the path to save the ZIP file
  zip_path <- if (keep_zip) {
    # Save the ZIP file to the specified extraction directory
    file.path(extract_to, basename(url))
  } else {
    # Use a temporary file path for the ZIP file
    tempfile(fileext = ".zip")
  }
  
  # Ensure temporary file cleanup if there's an error and keep_zip is FALSE
  on.exit({
    if (!keep_zip && file.exists(zip_path)) {
      unlink(zip_path)
    }
  }, add = TRUE)
  
  # Attempt to download the ZIP file
  tryCatch({
    download.file(url, zip_path, mode = "wb")
  }, error = function(e) {
    stop("Failed to download the file from the specified URL: ", e$message)
  })
  
  # Attempt to unzip the file to the specified extraction directory
  tryCatch({
    unzip(zip_path, exdir = extract_to)
  }, error = function(e) {
    stop("Failed to unzip the file: ", e$message)
  })
  
  # Delete the ZIP file if 'keep_zip' is FALSE
  if (!keep_zip) {
    unlink(zip_path)
  }
  
  gc()
  
  invisible(NULL)
}

#' Merge a List of Raster Files and Optionally Write to Disk
#'
#' Merges a list of raster files into a single raster object. The merged raster can either
#' be saved to a specified file path or returned as an in-memory object.
#'
#' @param file_list Character vector. A list of file paths to the raster files to be merged.
#' @param file_final_path Character. The file path where the merged raster will be saved if `write = TRUE`.
#' @param datatype Character. The data type of the output raster. Defaults to `"INT2U"`.
#' @param compress Logical. If `TRUE`, compresses the output file with DEFLATE compression when writing to disk. Defaults to `TRUE`.
#' @param write Logical. If `TRUE`, writes the merged raster to `file_final_path`. If `FALSE`, returns the merged raster in memory. Defaults to `TRUE`.
#'
#' @return If `write = TRUE`, returns invisible `NULL` after writing to disk. If `write = FALSE`, returns the merged raster object.
#'
#' @details This function reads a list of raster files, merges them, and either writes the merged raster to a specified path
#' or returns it in memory. Compression is available when writing to disk to reduce file size.
#'
#' @importFrom purrr map
#' @importFrom terra rast sprc merge writeRaster
#' @export
#'
#' @examples
#' \dontrun{
#' file_paths <- c("path/to/raster1.tif", "path/to/raster2.tif")
#' # To write to disk
#' merge_list_of_rasters(file_paths, "path/to/final_raster.tif", datatype = "FLT4S", compress = TRUE, write = TRUE)
#' # To return in memory
#' merged_raster <- merge_list_of_rasters(file_paths, write = FALSE)
#' }
merge_list_of_rasters <- function(file_list, file_final_path = NULL, datatype = "INT2U", compress = TRUE, write = TRUE) {
  # Validate inputs
  if (!is.character(file_list) || length(file_list) < 1) stop("`file_list` must be a non-empty character vector.")
  if (write && (is.null(file_final_path) || !is.character(file_final_path) || length(file_final_path) != 1)) {
    stop("When `write = TRUE`, `file_final_path` must be a single, non-null character string.")
  }
  if (!is.logical(compress) || length(compress) != 1) stop("`compress` must be a single logical value.")
  if (!is.logical(write) || length(write) != 1) stop("`write` must be a single logical value.")
  
  # Load and merge the rasters
  combined_rasters <- file_list |>
    purrr::map(~ terra::rast(.x)) |>
    terra::sprc() |>
    terra::merge()
  
  # Write or return the merged raster
  if (write) {
    if (compress) {
      terra::writeRaster(combined_rasters,
                         file_final_path,
                         datatype = datatype,
                         gdal = c("COMPRESS=DEFLATE"))
    } else {
      terra::writeRaster(combined_rasters,
                         file_final_path,
                         datatype = datatype)
    }
    invisible(NULL)  # Return NULL after writing to disk
  } else {
    return(combined_rasters)  # Return the merged raster in memory
  }
}


## ArcGIS REST Data access


#' Fetch Data from an ArcGIS REST API Endpoint with Pagination
#'
#' This function retrieves geojson data from an ArcGIS REST API endpoint using pagination. It supports fetching a specified
#' number of entries or all available entries from the API endpoint. Written with ChatGPT 4o assistance.
#'
#' @param base_url A character string. The base URL of the ArcGIS REST API endpoint.
#' @param query_params A list. Query parameters to be used in the API request. The list should contain the necessary
#' parameters required by the API, such as `where`, `outFields`, and `f`.
#' @param max_record An integer. The maximum number of records that can be fetched in a single API request. This value is
#' usually defined by the ArcGIS REST API server limitations.
#' @param n An integer or character. Specifies the total number of entries to fetch. If `"all"`, the function fetches
#' all available records from the API. If an integer, it specifies the exact number of records to fetch.
#' @param timeout An integer. The time in seconds to wait before timing out the request.
#'
#' @return An `sf` object. A Simple Features (sf) object containing the fetched data.
#' @import httr sf
#' @examples
#' \dontrun{
#' base_url <- "https://example.com/arcgis/rest/services/your_service/FeatureServer/0/query"
#' query_params <- list(where = "1=1", outFields = "*", f = "geojson")
#' max_record <- 100
#' n <- 500  # Can also be "all"
#' result <- get.x.from.arcgis.rest.api(base_url, query_params, max_record, n)
#' print(result)
#' }
#' @importFrom httr GET status_code content timeout
#' @importFrom sf st_read
#' @export
access_data_get_x_from_arcgis_rest_api_geojson <- function(base_url, query_params, max_record, n, timeout) {
  # Input validation
  if (!is.character(base_url) || length(base_url) != 1) {
    stop("Parameter 'base_url' must be a single character string.")
  }
  if (!is.list(query_params)) {
    stop("Parameter 'query_params' must be a list.")
  }
  if (!is.numeric(max_record) || length(max_record) != 1 || max_record <= 0) {
    stop("Parameter 'max_record' must be a positive integer.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("Parameter 'timeout' must be a positive integer.")
  }
  
  
  # Initialize variables
  total_features <- list()
  offset <- 0
  total_fetched <- 0  # Keep track of the total number of records fetched
  
  # Determine the limit for fetching records
  fetch_all <- FALSE
  if (n == "all") {
    fetch_all <- TRUE
  } else if (!is.numeric(n) || n <= 0) {
    stop("Parameter 'n' must be a positive integer or 'all'.")
  }
  
  repeat {
    # Update the resultOffset parameter in query_params
    query_params$resultOffset <- offset
    
    # Make the GET request using the base URL and query parameters
    response <- httr::GET(url = base_url, query = query_params, httr::timeout(timeout))
    
    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      # Read the GeoJSON data directly into an sf object
      data <- sf::st_read(httr::content(response, as = "text"), quiet = TRUE)
      
      # Append the data to the list of features
      total_features <- append(total_features, list(data))
      
      # Update the total number of fetched records
      total_fetched <- total_fetched + nrow(data)
      
      # Provide user feedback for long-running processes
      cat(sprintf("Fetched %d records so far...\n", total_fetched))
      
      # Determine if we should stop fetching
      if ((nrow(data) < max_record) || (!fetch_all && total_fetched >= n)) {
        break
      }
      
      # Increment the offset by the maximum number of records for the next page
      offset <- offset + max_record
    } else {
      # Handle errors and provide meaningful messages
      error_message <- httr::content(response, "text", encoding = "UTF-8")
      stop("Failed to fetch data: ", httr::status_code(response), " - ", error_message)
    }
  }
  
  # Combine all pages into one sf object
  all_data_sf <- do.call(rbind, total_features)
  
  # If n is not "all", limit the output to the first n records
  if (!fetch_all) {
    all_data_sf <- all_data_sf[1:min(n, nrow(all_data_sf)), ]
  }
  
  return(all_data_sf)
}


#' Extract Topographic Features for an sf Object
#'
#' This function extracts topographic features, including elevation, slope, and aspect, 
#' for a given `sf` object. The function allows extracting values at the centroid of each 
#' feature or aggregating values over the entire feature using `exactextractr`.
#'
#' @param sf_set An `sf` object representing spatial features.
#' @param centroid Logical. If `TRUE`, extracts topographic values at the centroid of 
#'   each feature; otherwise, values are aggregated over the entire feature using `exact_extract`. 
#'   Default is `TRUE`.
#' @param z Integer. The zoom level for the elevation raster retrieved using `elevatr::get_elev_raster()`. 
#'   Higher values provide higher resolution. Default is `12` (~30m resolution).
#' @param ... Additional arguments passed to `exact_extract()` when `centroid = FALSE`.
#'
#' @return An `sf` object with added columns:
#'   - `elevation`: Elevation values (meters).
#'   - `slope`: Slope values (degrees).
#'   - `aspect`: Aspect values (degrees, where 0° = North).
#'
#' @details
#' - When `centroid = TRUE`, the function computes the centroid of each feature and extracts 
#'   the elevation, slope, and aspect values at that point.
#' - When `centroid = FALSE`, `exact_extract()` is used to aggregate values over each feature.
#' - The function internally converts the elevation raster to a `terra` raster for slope 
#'   and aspect calculations.
#'
#' @importFrom sf st_bbox st_crs st_centroid
#' @importFrom elevatr get_elev_raster
#' @importFrom terra rast terrain extract
#' @importFrom exactextractr exact_extract
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(elevatr)
#' library(terra)
#' library(exactextractr)
#' library(dplyr)
#' 
#' # Example sf object (polygon)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' 
#' # Extract topographic features at centroids
#' nc_topo <- extract_topo(nc, centroid = TRUE)
#' 
#' # Extract topographic features using exact extraction
#' nc_topo2 <- extract_topo(nc, centroid = FALSE, fun = mean)
#' }
#'
#' @export
extract_topo <- function(sf_set,
                         centroid = TRUE,
                         z = 12, #~30m
                         ...) {
  
  # Get bounding box and fetch elevation raster
  bbox <- sf::st_bbox(sf_set)
  elevation_raster <- elevatr::get_elev_raster(locations = bbox,
                                               z = z,
                                               prj = sf::st_crs(sf_set)) #~30m res
  
  # Convert to terra raster for processing
  elevation_rast <- terra::rast(elevation_raster)
  
  # Calculate slope and aspect
  slope <- terra::terrain(elevation_rast, v = "slope", unit = "degrees")
  aspect <- terra::terrain(elevation_rast, v = "aspect", unit = "degrees")
  
  if(centroid) {
    
    # Compute centroids and extract values
    centroids <- sf::st_centroid(sf_set)
    extracted_vals <- cbind(
      terra::extract(elevation_rast, centroids, ID = FALSE),
      terra::extract(slope, centroids, ID = FALSE),
      terra::extract(aspect, centroids, ID = FALSE)
    )
    
    # Add extracted values to sf object
    sf_set_plus <- sf_set %>%
      mutate(elevation = extracted_vals[,1],
             slope = extracted_vals[,2],
             aspect = extracted_vals[,3])
    
  } else {
    
    # Extract values from polygons using exactextractr
    sf_set_plus <- sf_set |>
      dplyr::mutate(
        elevation = exactextractr::exact_extract(elevation_rast, sf_set, ...),
        slope = exactextractr::exact_extract(slope, sf_set, ...),
        aspect = exactextractr::exact_extract(aspect, sf_set, ...)
      )
    
  }
  
  return(sf_set_plus)
}
