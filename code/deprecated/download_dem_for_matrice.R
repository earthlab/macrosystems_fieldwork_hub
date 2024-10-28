


#########################
# NOW DEPRECATED, see generate_plot_boundaries_anywhere.R
########################

# 
# library(elevatr)
# 
# #Load plot points
# plotPoints <- sf::st_read(here::here('data', 'sampling_options', 'kremmling_interest.gpkg')) |>
#   sf::st_transform("EPSG:5070")
# 
# x <- elevatr::get_elev_raster(plotPoints, z = 12) |> #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#Key_information_about_version_0990_and_upcoming_versions_of_elevatr
#   terra::rast()
# plot(x)
# plot(plotPoints, add = TRUE)
# 
# x <- x |> terra::project(terra::crs('EPSG:4236'))
# 
# terra::writeRaster(x, here::here('data', 'fieldwork', 'dems', 'kremmling_dem.tif'), overwrite = TRUE)
# 
