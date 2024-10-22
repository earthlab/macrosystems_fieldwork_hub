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


# Data access ----

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
#' 
#' @importFrom terra rast
#' @export
access_landfire_evt_conus_2022 <- function() {
  lf_evt <- paste0(
    "/vsizip/vsicurl/", #magic remote connection
    "https://landfire.gov/data-downloads/US_230/LF2022_EVT_230_CONUS.zip", #copied link to download location
    "/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif") |> #path inside zip file
    terra::rast()
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
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/}
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
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip",
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
#' @source
#' U.S. EPA Ecoregions Data: \url{https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/}
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
    "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip",
    "/us_eco_l3.shp"
  ) |>
    sf::st_read()
  
  return(epa_l3)
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





### NPS Data ----






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



# Utility ----


#' Install and Load Required Packages Using pak
#'
#' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' If any packages are missing, it offers to install them automatically or asks for user permission.
#' It uses the `pak` package for faster and more efficient package installation.
#'
#' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' GitHub packages should be specified as `username/repo` in strings.
#' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' without asking for user permission. If "n", asks for permission from the user.
#' @return No return value. Installs and loads the specified packages as needed.
#' @examples
#' \dontrun{
#' install_and_load_packages(c(dplyr, here, "username/repo"))
#' }
#' @importFrom pak pkg_install
#' @export
install_and_load_packages <- function(package_list, auto_install = "n") {
  # Convert non-string package names to strings
  package_list <- lapply(package_list, function(pkg) {
    if (is.symbol(pkg)) {
      deparse(substitute(pkg))
    } else {
      pkg
    }
  })
  
  # Check if pak is installed; install if not
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("The 'pak' package is required for fast installation of packages.\n")
    response <- if (auto_install == "y") "y" else readline(prompt = "\nDo you want to install the 'pak' package? (y/n): ")
    if (tolower(response) == "y") {
      install.packages("pak")
    } else {
      stop("Installation cannot proceed without 'pak'. Please install it manually and rerun.")
    }
  }
  
  # Initialize lists to store missing CRAN and GitHub packages
  missing_cran_packages <- c()
  missing_github_packages <- c()
  
  # Helper function to get user input
  get_user_permission <- function(prompt_msg) {
    if (auto_install == "y") {
      return("y")
    } else {
      return(tolower(readline(prompt = prompt_msg)))
    }
  }
  
  # Check for missing packages
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
    } else { # CRAN package
      package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
    }
    if (!package_loaded) {
      if (grepl("/", pkg)) {
        missing_github_packages <- c(missing_github_packages, pkg)
      } else {
        missing_cran_packages <- c(missing_cran_packages, pkg)
      }
    }
  }
  
  # Install missing CRAN packages using pak::pkg_install
  if (length(missing_cran_packages) > 0) {
    cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_cran_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing CRAN packages.\n")
    }
  }
  
  # Install missing GitHub packages using pak::pkg_install
  if (length(missing_github_packages) > 0) {
    cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_github_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing GitHub packages.\n")
    }
  }
  
  # Load all packages after checking for installation
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      if (!require(package_name, character.only = TRUE)) {
        cat("Failed to load GitHub package:", package_name, "\n")
      }
    } else { # CRAN package
      if (!require(pkg, character.only = TRUE)) {
        cat("Failed to load CRAN package:", pkg, "\n")
      }
    }
  }
  
  cat("All specified packages installed and loaded.\n")
}


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
# create.qgis.style.for.paletted.raster.from.csv(styleData, "path/to/output.qml", "value", "label", "hex")
# Citation:
#   Function authored by R Code Stylist, GPT-4, OpenAI, in collaboration with the user.
create.qgis.style.for.paletted.raster.from.csv <- function(styleData, outputQmlPath, valueColumn, labelColumn, colorScheme = "hex") {
  
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
