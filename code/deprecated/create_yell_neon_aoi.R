# Create YELL NEON AOP AOI

library(osmdata)
library(here)
library(sf)
library(terra)
library(tidyverse)
library(mapview)

#open data necessary & prep

yellAOP <- sf::st_read(here::here('data/NEON_AOP_FlightBoxes/YELL_NEON_AOP_FlightBox.shp')) |>
  sf::st_transform(crs = 4326)
#the BMA data has some funk in its trunk. fix after opening
bma <- sf::st_read(here::here('data/yell_BearManagementAreas/yell_BearManagementAreas.shp')) |>
  sf::st_transform(crs = 4326) |>
  sf::st_zm(z = FALSE, m = FALSE)
roadsData <- osmdata::opq(bbox = sf::st_bbox(yellAOP)) |>
  osmdata::add_osm_feature(key = "highway",
                           key_exact = FALSE,
                           value_exact = FALSE,
                           match_case = FALSE) |>
  osmdata::osmdata_sf()
roads <- roadsData$osm_lines |>
  dplyr::filter(highway != "path") |>
  dplyr::mutate(group = 1) |>
  group_by(group) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup()

#mapview(yellAOP) + mapview(bma) + mapview(roads)

#create aoi
aoi <- roads |>
  sf::st_buffer(units::set_units(2, "miles"), nQuadSegs = 100) |> #buffer roads
  sf::st_intersection(yellAOP) |> #clip to NEON AOP
  sf::st_difference(bma |>
                      dplyr::filter(NAME == "WASHBURN") |>
                      sf::st_buffer(units::set_units(0.1, "miles"))) # remove closed bma plus a small buffer around closed area


mapview(aoi) + mapview(bma)

sf::st_write(aoi, here::here('data/yell_aoi.gpkg'), append = FALSE)

#Function to write a shapefile to a new, file-specific directory and add a zipped version
#    shp = the sf file to write to shapefile
#    location = path of directory to create the new file-specific subdirectory in
#    filename = name of file WITHOUT .shp
#    zipOnly = TRUE / FALSE, should the original (unzipped) files be removed?

# Example use:
# st_write_shp(shp = prepped_for_parks_etal,
#              location = here("data/derived"),
#              filename = "career_lba_for_parks_v1",
#              zipOnly = TRUE)
st_write_shp <- function(shp, location, filename, zipOnly) {
  
  #Check for required packages and install if not installed, then load
  list.of.packages <- c("zip","sf","here")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(zip)
  library(sf)
  library(here)
  
  
  #Create subdirectory
  outDir <- here::here(location, filename)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  
  
  #Write file
  sf::st_write(shp,
               here::here(outDir, paste(filename, ".shp", sep = "")),
               append = FALSE) #overwrite
  
  #Get all shapefile components
  allShpNms <- list.files(here::here(outDir),
                          pattern = paste(filename, "*", sep = ""),
                          full.names = TRUE)
  
  #Zip together
  zip::zip(zipfile = here::here(outDir, paste(filename, ".zip", sep="")),
           files = allShpNms,
           mode = "cherry-pick")
  
  
  #Remove raw files if desired
  if(zipOnly == TRUE) {
    file.copy(here(outDir, paste(filename, ".zip", sep="")), here::here(location, paste(filename, ".zip", sep="")))
    unlink(here(outDir), recursive = TRUE)          
  }
  
}

st_write_shp(aoi,
             location = here::here('data'),
             filename = "yell_aoi",
             zipOnly = TRUE)




evt <- terra::rast(here::here('data/existing-vegetation-type/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif'))





#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped to vector
#Returns raster in the same form that it came in
# PARAMETERS
# raster : a SpatRaster, PackedSpatRaster, RasterLayer, RasterStack, or RasterBrick object
# vector : a SpatVector, PackedSpatVector or SF object
# mask : TRUE or FALSE; whether terra::clip should mask the raster as well
crop.careful.universal <- function(raster, vector, mask, verbose = FALSE) {
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


evtAOI <- evt |> crop.careful.universal(vector = aoi, mask = FALSE, verbose = TRUE)

terra::plot(evtAOI, mar = c(2,2,2,25))

terra::writeRaster(evtAOI, here::here('data/existing-vegetation-type/evt_yell.tif'))
