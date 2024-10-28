
#############################
# NOW DEPRECATED - SEE generate_plot_boundaries_anywhere.R
##############################

# 
# library(raster)
# library(sp)
# library(sf)
# library(dplyr)
# library(tidyr)
# 
# setwd("C:/Users/skyge/OneDrive/Documents/GRAD/Macrosystems/Macrosystems_R/LandsatARD")
# projdir <- ("C:/Users/skyge/OneDrive/Documents/GRAD/Macrosystems/Macrosystems_R/LandsatARD")
# ARDbands <- list.files(projdir, pattern = "^LC08.*SR_B.*\\.TIF$")
# 
# ARDraster_list <- list()
# for (i in 1:length(ARDbands)) {
#   ARDraster_list[[i]] <- raster(ARDbands[i])
# }
# 
# ARDstack <- stack(ARDraster_list) ##Example Landsat ARD raster
# ARDstack <- projectRaster(ARDstack , crs="EPSG:5070")
# plotcoords <- sampleRandom(ARDstack, 5, xy = TRUE, sp=TRUE, na.rm = TRUE)  ##Random example coordinates, structured with x and y columns
# 
# ##Get position of corresponding pixel on the raster for each coordinates
# cell_index <- list()
# row <- list()
# col <- list()
# for (i in 1: length(plotcoords)){
#   cell_index[i] <- cellFromXY(ARDstack, c(plotcoords[i,]$x, plotcoords[i,]$y))
#   row[i] <- rowFromCell(ARDstack, unlist(cell_index[i]))
#   col[i] <- colFromCell(ARDstack, unlist(cell_index[i]))
# }
# 
# ##Function to get the extent of pixels
# get_pixel_extent <- function(row, col, ARDstack) {
#   cell_res <- res(ARDstack)  # Get cell resolution
#   x_min <- extent(ARDstack)[1] + cell_res[1] * (col - 1)
#   y_max <- extent(ARDstack)[4] - cell_res[2] * (row - 1)
#   x_max <- x_min + cell_res[1]
#   y_min <- y_max - cell_res[2]
#   return(c(x_min, x_max, y_min, y_max))
# }
# 
# ##Find extents of neighboring pixels
# neighboring_extents <- list()
# for (i in 1:length(row)) {
#   neighboring_extents[[i]] <- list()
#   for (j in c(-1, 0, 1)) {
#     for (k in c(-1, 0, 1)) {
#       ##Row and column index of the neighboring pixel for each offset
#       neighboring_row <- row[[i]] + j
#       neighboring_col <- col[[i]] + k
#       neighboring_extents[[i]][[length(neighboring_extents[[i]]) + 1]] <- get_pixel_extent(neighboring_row, neighboring_col, ARDstack)
#     }
#   }
# }
# 
# neighbor_df <- data.frame()
# for (i in seq_along(neighboring_extents)) {
#   for (j in seq_along(neighboring_extents[[i]])) {
#     extent <- neighboring_extents[[i]][[j]]
#     xmin <- extent[1]
#     xmax <- extent[2]
#     ymin <- extent[3]
#     ymax <- extent[4]
#     neighbor_df <- rbind(neighbor_df, c(i, xmin, xmax, ymin, ymax))
#   }
# }
# colnames(neighbor_df) <- c("ID", "xmin", "xmax", "ymin", "ymax")
# 
# plot_extents <- neighbor_df %>% group_by(ID) %>% summarise(xmin = min(xmin), xmax = max(xmax), ymin = min(ymin), ymax = max(ymax)) %>% as.data.frame()
# 
# plot_polygons <- apply(plot_extents[2:5], 1, FUN = function(x) {
#   poly <- as(extent(x), "SpatialPolygons")
#   poly@polygons[[1]]@ID <- as.character(uuid::UUIDgenerate())
#   return(poly)
# })
# 
# plotpolys_combined <- do.call(rbind, plot_polygons)
# 
# plot_sf <- st_as_sf(plotpolys_combined)##Final output is simple feature collection
# st_crs(plot_sf) <- "EPSG:5070"
# 
# #st_write(plot_sf, "plot_boundaries.gpkg", driver = "GPKG", append = F)
# 
# #plot(ARDstack)
# #plot(plot_sf, add=T)