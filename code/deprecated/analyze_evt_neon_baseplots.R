# A script to get the presence of raw EVT classes within NEON Base plots

library(sf)
library(terra)
library(dplyr)

# OPERATE ----

epsg <- "EPSG:5070" #Albers equal area

evt <- terra::rast(here::here('data', 'existing-vegetation-type', 'LF2020_EVT_220_CONUS', 'Tif', 'LC20_EVT_220.tif'))


neonPlotsAll <- glue::glue(
  "/vsizip/vsicurl/", #magic remote connection
  "https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V10.zip", #copied link to download location
  "/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Polygons_V10.shp") |> #path inside zip file
  sf::st_read() |>
  sf::st_transform(epsg)



# Function to get presence of a raster value within NEON base plots
# PARAMETERS
# siteID :: the NEON ID of interest (e.g. "YELL")
# lc :: a raster to pull values from (intended to be a landcover categorical raster), in SpatRaster format
get.base.plot.freqs <- function(siteID, lc) {
  basePlots <- neonPlotsAll |>
    dplyr::filter(subtype == "basePlot" & siteID == siteID)
  bpRast <- lc |>
    terra::crop(y = basePlots, mask = TRUE, touches = FALSE)
  bpFreqs <- terra::freq(bpRast)  
  
  return(bpFreqs)
}



wrefBPFreqs <- get.base.plot.freqs(siteID = "WREF", lc = evt)
yellBpFreqs <- get.base.plot.freqs(siteID = "YELL", lc = evt)
