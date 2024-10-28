
library(sf)
library(here)
library(glue)
library(dplyr)


# Load data ----

epsg <- "EPSG:4326"


aopFile <- here::here('data', 'AOP_flightboxesAllSites.gpkg')
if(file.exists(aopFile)) {
  aopAll <- sf::st_read(aopFile) |>
    sf::st_transform(epsg)
} else {
  aopAll <- glue::glue(
    "/vsizip/vsicurl/", #magic remote connection
    "https://www.neonscience.org/sites/default/files/AOP_flightBoxes_0.zip", #copied link to download location
    "/AOP_flightBoxes/AOP_flightboxesAllSites.shp") |> #path inside zip file
    sf::st_read() |>
    sf::st_transform(epsg)
  sf::st_write(aopAll, aopFile)
}


# FUNCTIONS ----


# Helper function to determine the appropriate NAD83 UTM zone EPSG code for a given polygon
find.nad83.utm.epsg <- function(polygon) {
  # Ensure the polygon is in WGS84 (EPSG:4326) for accurate longitude calculation
  polygonWgs84 <- st_transform(polygon, crs = 4326)
  
  # Calculate the centroid
  centroid <- st_centroid(polygonWgs84)
  centroidCoords <- st_coordinates(centroid)
  
  # Get the longitude of the centroid
  longitude <- centroidCoords[1, "X"]
  
  # Calculate the UTM zone
  utmZone <- (floor((longitude + 180) / 6) %% 60) + 1
  
  # Determine the EPSG code for NAD83 UTM zone
  epsgCode <- 26900 + utmZone
  
  return(epsgCode)
}


# A function to generate a gridded set of polygons representing AOP image tiles for a NEON AOP footprint (the given aoi)
generate.aop.tile.polygons <- function(aoi) {
  # Find the NAD83 UTM EPSG code for the AOI
  epsgCode <- find.nad83.utm.epsg(aoi)
  
  # Transform the AOI to the appropriate UTM CRS
  aoiUtm <- st_transform(aoi, crs = epsgCode)
  
  # Get the bounding box of the AOI
  bbox <- st_bbox(aoiUtm)
  
  # Define the range of coordinates for the grid
  xCoords <- seq(floor(bbox["xmin"] / 1000) * 1000, ceiling(bbox["xmax"] / 1000) * 1000, by = 1000)
  yCoords <- seq(floor(bbox["ymin"] / 1000) * 1000, ceiling(bbox["ymax"] / 1000) * 1000, by = 1000)
  
  # Create the grid of polygons
  polygons <- list()
  idIndex <- 1
  
  for (x in xCoords) {
    for (y in yCoords) {
      # Create a 1km by 1km polygon
      polygon <- st_polygon(list(matrix(c(x, y, 
                                          x + 1000, y, 
                                          x + 1000, y + 1000, 
                                          x, y + 1000, 
                                          x, y), 
                                        ncol = 2, byrow = TRUE)))
      
      # Create an sf object for the polygon with an id
      polygons[[idIndex]] <- st_sf(id = paste0("(", x, ",", y, ")"), geometry = st_sfc(polygon, crs = epsgCode))
      idIndex <- idIndex + 1
    }
  }
  
  # Combine all polygons into a single sf object using rbind
  grid <- do.call(rbind, polygons)
  
  # Clip the grid to the AOI
  gridClipped <- st_intersection(grid, aoiUtm)
  
  return(gridClipped)
}


# Operational function to create and write out a polygon set with the names of all image tiles within an AOP footprint
generate.aop.image.grid <- function(site, year, visit, outDir) {
  
  aop <- aopAll |>
    dplyr::filter(siteID == site)
  
  siteGrid <- generate.aop.tile.polygons(aop) |>
    dplyr::mutate(tileName = paste0(year, "_", site, "_", visit, "_", id, "_image"))
  
  return(siteGrid)
}

# OPERATE ----

x <- generate.aop.image.grid(site = "YELL",
                        year = 2023,
                        visit = 5,
                        outDir = here::here('data', 'derived'))


