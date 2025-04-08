# This script creates figures and tables describing the Macrosystems project data
# It will AUTOMATICALLY install and load any missing packages
# Tyler L. McIntosh
# 2/13/25

rm(list = ls())

# Load necessary libraries
library(here)

source(here::here('code/functions.R'))

install_and_load_packages(c("sf",
                            "here",
                            "tidyverse",
                            "elevatr",
                            "terra",
                            "exactextractr",
                            "purrr",
                            "kableExtra",
                            "rlang",
                            "tigris",
                            "webshot2",
                            "RColorBrewer",
                            "scico",
                            "patchwork",
                            "extrafont",
                            "ggpubr"))

# load Calibri font for use in R
if(!"Calibri" %in% windowsFonts()$Calibri) {
  tryCatch(
    { 
      extrafont::font_import(pattern = "calibri")
      extrafont::loadfonts(device = "win", quiet = TRUE)
    },
    error = function(cond) {
      message("Calibri was not installed as it is not available on this computer; figures will be in default font")
      NA
    }
  )
}

# Robust function not working for some reason, unknown why; storing for future
# check_and_load_font <- function(font_name) {
#   # Convert input font name and registered font names to lowercase for case-insensitive comparison
#   available_fonts <- tolower(names(windowsFonts()))
#   font_name_lower <- tolower(font_name)
#   
#   # Check if the font is already registered
#   if (!font_name_lower %in% available_fonts) {
#     tryCatch(
#       {
#         message(paste("Attempting to install and load", font_name, "..."))
#         
#         # Import and load the font
#         extrafont::font_import(pattern = font_name, prompt = FALSE)
#         extrafont::loadfonts(device = "win", quiet = TRUE)
#         
#         message(paste(font_name, "successfully installed and loaded!"))
#       },
#       error = function(cond) {
#         message(paste(font_name, "was not installed as it is not available on this computer."))
#         message("Figures will be rendered in the default font.")
#         message("Here's the original error message:")
#         message(conditionMessage(cond))
#         return(NA)
#       }
#     )
#   } else {
#     message(paste(font_name, "is already available."))
#   }
# }


# Set EPSG
epsg <- 5070

# Function to load and transform an sf object
load_transform_sf <- function(file_path, dataset_name, epsg) {
  sf_object <- st_read(here(file_path)) %>%
    st_transform(crs = epsg) %>%
    mutate(source_dataset = dataset_name) # Add dataset origin
  return(sf_object)
}

# Load, transform, prepare datasets ----
drone <- load_transform_sf("data/derived/uas_polygons_2_14_2025_analysis_ready.geojson", "uas", epsg) |>
  mutate(extraction_group = plotID_clean) |>
  select(cover_category, cover_subcategory, extraction_group, ecoregion, source_dataset)
aop_field <- load_transform_sf("data/derived/aop_polygons_2_14_2025_analysis_ready.geojson", "aop_field", epsg)  |>
  mutate(extraction_group = aop_site) |>
  select(cover_category, cover_subcategory, extraction_group, ecoregion, source_dataset)
aop_trees <- load_transform_sf("data/derived/ard_weinstein_trees.geojson", "aop_trees", epsg) |>
  mutate(extraction_group = siteID,
         ecoregion = case_when(siteID == "YELL" ~ "MiddleRockies",
                               siteID == "RMNP" ~ "SouthernRockies",
                               siteID == "NIWO" ~ "SouthernRockies",
                               siteID == "WREF" ~ "Cascades",
                               TRUE ~ NA)) |>
  select(cover_category, cover_subcategory, extraction_group, ecoregion, source_dataset)
plots <- load_transform_sf("data/manual/macrosystems_plots_23_24.geojson", "uas_plots", epsg)

# Merge & get area
all <- rbind(drone,
             aop_trees,
             aop_field) |>
  st_area_to_poly(nm = area_m2)


# Access Landfire EVT data, ecoregions, AOP

evt <- access_landfire_evt_conus_2022(access = "download",
                                      dir_path = here::here("data/raw"))
evt_csv <- access_landfire_evt_conus_2022_csv()

epa_l3 <- access_data_epa_l3_ecoregions_vsi() |>
  sf::st_transform(epsg)

neon_aop <- access_neon_aop_flight_box_data() |>
  sf::st_transform(epsg)


# Add data on other variables ----
# Specialized function to add both EVT and topo data to the polygons
extract_vars <- function(sf_subset) {
  sf_subset <- extract_topo(sf_subset,
                            centroid = FALSE,
                            fun = "mean")
  
  lf_interest <- terra::crop(evt, sf_subset |> sf::st_bbox())
  
  # Compute centroids and extract values
  centroids <- sf::st_centroid(sf_subset)
  extracted_vals <- terra::extract(evt, centroids, ID = FALSE)
  
  # Add extracted values to sf object
  sf_subset_plus <- sf_subset |>
    dplyr::mutate(evt = extracted_vals[,1]) |>
    dplyr::left_join(evt_csv, by = dplyr::join_by(evt == EVT_NAME))
  
  return(sf_subset_plus)
}

# Apply extract_vars to each extraction group (in parallel) and merge results
all_dats_added <- all |>
  dplyr::group_split(extraction_group) |>   # Split by site
  furrr::future_map_dfr(extract_vars) |>   # Apply function and merge results
  dplyr::mutate(all = "all_data") |>
  dplyr::mutate(ecoregion = str_replace_all(ecoregion, "(?<=.)([A-Z])", " \\1"))

all_dats_added <- all_dats_added |>
  dplyr::mutate(source_dataset = factor(source_dataset, levels = c("uas", "aop_field", "aop_trees")))

# Calculate summary statistics & graphics ----

## Topo ----

create_density_plots <- function(x, nm) {
  gg <- ggplot(data = all_dats_added) +
    # Density plot for source_dataset using RColorBrewer's Set2 palette
    geom_density(aes(x = {{x}}, color = source_dataset), size = 1) +
    # Density plot for "all" dataset, ensuring it appears in the legend
    geom_density(aes(x = {{x}}, color = "all_data"), size = 2) +
    
    # Manually set colors for both source_dataset and "All"
    scale_color_manual(
      name = "Data Type",  # Legend title
      values = c(
        # "all_data"  = brewer.pal(8, "Set2")[4],
        # "aop_field" = brewer.pal(8, "Set2")[1],  # First color from Set2
        # "aop_trees" = brewer.pal(8, "Set2")[2],  # Second color from Set2
        # "uas"       = brewer.pal(8, "Set2")[3]   # Third color from Set2
        "all_data"  = brewer.pal(8, "Dark2")[4],
        "aop_field" = brewer.pal(8, "Dark2")[1],  # First color from Set2
        "aop_trees" = brewer.pal(8, "Dark2")[2],  # Second color from Set2
        "uas"       = brewer.pal(8, "Dark2")[3]   # Third color from Set2
        # "all_data"  = scico::scico(4, palette = "batlow")[4],
        # "aop_field" = scico::scico(4, palette = "batlow")[3],  
        # "aop_trees" = scico::scico(4, palette = "batlow")[2],  
        # "uas"       = scico::scico(4, palette = "batlow")[1]  
      ),
      labels = c(
        "all_data"  = "All",
        "aop_field" = "NEON AOP Cover Digitization",
        "aop_trees" = "NEON Woody Vegetation \nSurveys",
        "uas"       = "UAS Cover Digitization"
      ),
      breaks = c("all_data", "uas", "aop_field", "aop_trees")
    ) +
    
    # Axis labels
    labs(
      x = tools::toTitleCase(deparse(substitute(x))), # Capitalize first letter of X label
      y = "Density"
    ) +
    
    # Theme adjustments for better visualization
    theme_minimal() +
    theme(
      legend.position = "right", # Keep legend visible
      #legend.position = "none",
      legend.title = element_text(face = "bold",
                                  size = 17),
      legend.text = element_text(size = 13),
      text = element_text(family = "Calibri")
    )
  
  # Faceted version
  gg_facet <- gg + facet_wrap(~ ecoregion)
  
  # Save both versions
  ggsave(filename = here("figs", paste0("field_data_summary_", nm, ".jpg")),
         plot = gg, width = 8, height = 6, dpi = 300)
  ggsave(filename = here("figs", paste0("field_data_summary_facet_", nm, ".jpg")),
         plot = gg_facet, width = 8, height = 6, dpi = 300)

  return(list(gg, gg_facet))
}

create_hist_plots <- function(x, nm, slope = FALSE) {
  
  bin_width <- diff(range(all_dats_added[[deparse(substitute(x))]], na.rm = TRUE)) / 30  
  total_count <- nrow(all_dats_added)
  
  gg <- ggplot(data = all_dats_added) +
    geom_histogram(aes(x = {{x}},
                       y = after_stat(count),
                       fill = source_dataset)) +
    # geom_density(aes(x = {{x}},
    #                  color = "Density: All data",
    #                  y = (after_stat(density) * bin_width * total_count) / 2),
    #              size = 1) +

    # Manually set colors for both source_dataset and "All"
    scale_fill_manual(
      name = "Data Type",  # Legend title
      values = c(
        "aop_field" = scico::scico(10, palette = "batlow", categorical = TRUE)[4],
        "aop_trees" = scico::scico(10, palette = "batlow", categorical = TRUE)[1],
        "uas"       = scico::scico(10, palette = "batlow", categorical = TRUE)[3]
      ),
      labels = c(
        "aop_field" = "Airborne imaging-assisted",
        "aop_trees" = "Woody vegetation survey",
        "uas"       = "Drone plots"
      )    ) +
    
    scale_color_manual(
      name = "",
      values = c(scico::scico(10, palette = "batlow", categorical = TRUE)[5])
    ) +
    
    # Order the legends
    guides(
      fill  = guide_legend(order = 1,
                           override.aes = list(color = NA)),  # "Data Type" legend first
      color = guide_legend(order = 2)   # Density legend second
    ) +
    
    # Axis labels
    labs(
      x = tools::toTitleCase(deparse(substitute(x))), # Capitalize first letter of X label
      y = "Count"
    ) +
    
    # Theme adjustments for better visualization
    theme_minimal() +
    theme(
      legend.position = "right", # Keep legend visible
      #legend.position = "none",
      legend.spacing = unit(-25, "pt"),
      legend.title = element_text(face = "bold",
                                  size = 17),
      legend.text = element_text(size = 13),
      legend.key = element_rect(color = NA),
      text = element_text(family = "Calibri")
    )
  
  if(slope == TRUE) {
    gg <- gg + xlim(0, 45)
  }
  
  gg_2 <- gg + geom_density(aes(x = {{x}},
                             color = "Density: All data",
                             y = (after_stat(density) * bin_width * total_count) / 2),
                         size = 1)
  
  # Faceted version
  gg_facet <- gg +
    facet_wrap(~ ecoregion) +
    geom_density(aes(x = {{x}},
                     color = "Density: All data",
                     y = (after_stat(density) * bin_width * total_count) / (2 * 3)),
                 size = 1)
  
  # Save both versions
  ggsave(filename = here("figs", paste0("field_data_summary_hist_", nm, ".jpg")),
         plot = gg_2, width = 8, height = 6, dpi = 300)
  ggsave(filename = here("figs", paste0("field_data_summary_hist_facet_", nm, ".jpg")),
         plot = gg_facet, width = 8, height = 6, dpi = 300)
  
  return(list(plain = gg_2, facet = gg_facet))
}

create_combined_graphics <- function(s, a, e, fig_type) {
  # Create combined graphics
  combined_graphic <- (s[[1]] + theme(legend.position = "none")) /
    (a[[1]] + 
       theme(legend.title = element_text(face = "plain"))) /
    (e[[1]] + theme(legend.position = "none"))
  
  combined_graphic
  ggsave(filename = here("figs", paste0("field_data_summary_all_", fig_type, ".jpg")),
         plot = combined_graphic, width = 8.5, height = 9, dpi = 300)
  
  facet_combined_graphic <- (s[[2]] + theme(legend.position = "none")) /
    a[[2]] /
    (e[[2]] + theme(legend.position = "none"))

  ggsave(filename = here("figs", paste0("field_data_summary_all_", fig_type, "_facet.jpg")),
         plot = facet_combined_graphic, width = 12, height = 12, dpi = 300)
  
  return(facet_combined_graphic)
}


# slope_dp <- create_density_plots(slope, "slope")
# aspect_dp <- create_density_plots(aspect, "aspect")
# elevation_dp <- create_density_plots(elevation, "elevation")

# create_combined_graphics(s = slope_dp,
#                          a = aspect_dp,
#                          e = elevation_dp,
#                          fig_type = "density")

slope_hist <- create_hist_plots(slope, "slope", slope = TRUE)
slope_hist$plain
aspect_hist <- create_hist_plots(aspect, "aspect")
elevation_hist <- create_hist_plots(elevation, "elevation")


comb_hist <- create_combined_graphics(s = slope_hist,
                         a = aspect_hist,
                         e = elevation_hist,
                         fig_type = "hist")


brewer.pal(8, "Dark2")
scico::scico(10, palette = "batlow", categorical = TRUE)

## PFT & EVT ----

create_bar_charts <- function(y, nm, height = 6) {
  gg <- ggplot(data = all_dats_added) +
    geom_bar(aes(y = {{y}}, fill = source_dataset)) +
    # scale_fill_brewer(name = "Source Dataset",
    #                   palette = "Dark2",
    #                   labels = c(
    #                     "aop_field" = "NEON AOP Fieldmapping",
    #                     "aop_trees" = "NEON Woody Veg Surveys",
    #                     "uas"       = "UAS Fieldmapping"
    #                   )) +
    scale_fill_manual(
      name = "Data Type",  # Legend title
      values = c(
        "aop_field" = scico::scico(10, palette = "batlow", categorical = TRUE)[4],
        "aop_trees" = scico::scico(10, palette = "batlow", categorical = TRUE)[1],
        "uas"       = scico::scico(10, palette = "batlow", categorical = TRUE)[3]
      ),
      labels = c(
        #"all_data"  = "All",
        "aop_field" = "Airborne imaging-assisted",
        "aop_trees" = "Woody vegetation survey",
        "uas"       = "Drone plots"
      )    ) +
    theme_minimal() +
    theme(text = element_text(family = "Calibri")) +
    labs(x = "Count", y = "Cover Category")
  gg_facet <- gg +
    facet_wrap(~ ecoregion)
  
  ggsave(filename = here("figs", paste0("field_data_summary_", nm, ".jpg")),
         plot = gg,
         width = height * 2, height = height, dpi = 300)
  ggsave(filename = here("figs", paste0("field_data_summary_facet_", nm, ".jpg")),
         plot = gg_facet,
         width = height * 3, height = height, dpi = 300)
  
  return(list(plain = gg, facet = gg_facet))
}

cov_cat_bar <- create_bar_charts(cover_category, "cover_category", height = 3)
evt_gp_bar <- create_bar_charts(EVT_GP_N, "EVT_GP_N")


# Another combined figure
combined_legend <- patchwork::wrap_elements(full = ggpubr::get_legend(slope_hist[[1]] +
                                                            theme(legend.position = "bottom",
                                                                  legend.spacing = unit(0, "pt"))))

comb_all <- ((cov_cat_bar$plain + theme(legend.position = "none")) + 
               slope_hist[[1]] + theme(legend.position = "none")) /
            (aspect_hist[[1]] + theme(legend.position = "none") + 
               elevation_hist[[1]] + theme(legend.position = "none")) /
  plot_spacer() +
  plot_annotation(tag_levels = c("A")) +
  patchwork::inset_element(p = combined_legend,
                           left = 0,
                           bottom = 0.75,
                           right = 1,
                           top = 0.25,
                           align_to = "full",
                           ignore_tag = TRUE) +
  #combined_legend +
  plot_layout(heights = c(3, 3, 0.5))
comb_all
ggsave(filename = here("figs", paste0("field_data_summary_all_mixed_.jpg")),
       plot = comb_all, width = 12, height = 8, dpi = 300)






# Create count summaries

create_count_summary_table <- function(cols, rows) {
  
  c_nm <- rlang::as_name(enquo(cols))
  r_nm <- rlang::as_name(enquo(rows))
  
  t <- all_dats_added |>
    sf::st_drop_geometry() |>
    group_by({{cols}}, {{rows}}) |>
    summarize(observations = n()) |>
    ungroup() |>
    pivot_wider(names_from = {{cols}},
                values_from = observations) %>%
    mutate(All = rowSums(across(2:length(names(.))), na.rm = TRUE)) %>%
    bind_rows(summarise_all(., ~if(is.numeric(.)) sum(., na.rm = TRUE) else "Total"))
  
  t[is.na(t)] <- 0
  
  kableExtra::kbl(t,
                  caption = paste0("Observation counts by ", c_nm, " X ", r_nm)) |>
    kableExtra::kable_classic(, full_width = F) |>
    kableExtra::save_kable(file = here("figs", paste0("n_field_data_summary_", c_nm, "_X_", r_nm, ".html")))
}

create_count_summary_table(cols = source_dataset,
                                rows = ecoregion)

create_count_summary_table(cols = cover_category,
                           rows = ecoregion)

create_count_summary_table(cols = ecoregion,
                           rows = EVT_GP_N)

create_count_summary_table(cols = cover_category,
                           rows = EVT_GP_N)

## Area ----
#Create area subset summaries

all_area_dataset_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(source_dataset, ecoregion) |>
  summarize(mean_area = mean(area_m2)) |>
  ungroup() |>
  pivot_wider(names_from = ecoregion,
              values_from = mean_area) |>
  mutate(subset_type = "Dataset") |>
  rename(subset = source_dataset)

all_area_pft_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(cover_category, ecoregion) |>
  summarize(mean_area = mean(area_m2)) |>
  ungroup() |>
  pivot_wider(names_from = ecoregion,
              values_from = mean_area) |>
  mutate(subset_type = "PFT") |>
  rename(subset = cover_category)

all_area_eco_summary <- rbind(all_area_dataset_eco,
                              all_area_pft_eco) |>
  select(subset_type, subset, Cascades, `Middle Rockies`, `Southern Rockies`)





# Create large area summary table
all_area_pft <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(cover_category) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "PFT") |>
  rename(subset = cover_category)

all_area_eco <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(ecoregion) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "Ecoregion") |>
  rename(subset = ecoregion)

all_area_dataset <- all_dats_added |>
  sf::st_drop_geometry() |>
  group_by(source_dataset) |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "Dataset") |>
  rename(subset = source_dataset)


all_area <- all_dats_added |>
  sf::st_drop_geometry() |>
  summarize(mean_area = mean(area_m2),
            median_area = median(area_m2)) |>
  ungroup() |>
  mutate(subset_type = "All",
         subset = "All")

all_area_summary <- rbind(all_area_pft,
                          all_area_eco,
                          all_area_dataset,
                          all_area) |>
  select(subset_type, subset, mean_area, median_area)


# Export kables
kableExtra::kbl(all_area_summary,
                col.names = c("Subset Type",
                              "Subset",
                              "Mean area (m^2)",
                              "Median area (m^2)"),
                caption = "Polygon area data by subsets") |>
  kable_classic(, full_width = F) |>
  save_kable(file = here("figs/area_summary.html"))

kableExtra::kbl(all_area_eco_summary,
                col.names = c("Subset Type",
                              "Subset",
                              "Cascades",
                              "Middle Rockies",
                              "Southern Rockies"),
                caption = "Polygon mean area in m^2 data by ecoregion subsets") |>
  kable_classic(, full_width = F) |>
  save_kable(file = here("figs/area_ecoregion_summary.html"))



## Geographic ----

# access geographic data and export to use in QGIS for carto

usa <- tigris::nation() |>
  sf::st_transform(epsg)

western_states <- tigris::states() |>
  sf::st_transform(epsg) |>
  dplyr::filter(STUSPS %in% c("CA", "NV", "WA", "OR", "ID", "MT", "WY", "CO", "UT", "AZ", "NM"))

states <- tigris::states() |>
  sf::st_transform(epsg)


epa_l3 <- access_data_epa_l3_ecoregions_vsi() |>
  sf::st_transform(epsg)

s_rockies <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Southern Rockies")
m_rockies <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Middle Rockies")
cascades <- epa_l3 |>
  dplyr::filter(NA_L3NAME == "Cascades")

neon_sites_interest <- neon_aop |>
  dplyr::filter(siteID == "YELL" |
                  siteID == "RMNP" |
                  siteID == "NIWO" |
                  siteID == "WREF")

neon_sites_interest_points <- neon_sites_interest |>
  dplyr::group_by(siteID) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  sf::st_centroid()

# yell <- neon_aop |>
#   dplyr::filter(siteID == "YELL")
# rmnp <- neon_aop |>
#   dplyr::filter(siteID == "RMNP")
# niwo <- neon_aop |>
#   dplyr::filter(siteID == "NIWO")
# wref <- neon_aop |>
#   dplyr::filter(siteID == "WREF")

uas_plots <- sf::st_read(here::here("data", "manual", "macrosystems_plots_23_24.geojson")) |>
  sf::st_transform(epsg) |>
  sf::st_centroid() %>%
  dplyr::mutate(fid = seq(1:nrow(.)))

all_poly_points <- all |>
  sf::st_centroid()

dir_field_fig_data <- here::here('data', 'raw', 'field_fig')
dir_ensure(dir_field_fig_data)
sf::st_write(usa, here::here(dir_field_fig_data, "usa.gpkg"), append = FALSE)
sf::st_write(western_states, here::here(dir_field_fig_data, "west_states.gpkg"), append = FALSE)
sf::st_write(states, here::here(dir_field_fig_data, "states.gpkg"), append = FALSE)
sf::st_write(m_rockies, here::here(dir_field_fig_data, "m_rockies.gpkg"), append = FALSE)
sf::st_write(s_rockies, here::here(dir_field_fig_data, "s_rockies.gpkg"), append = FALSE)
sf::st_write(cascades, here::here(dir_field_fig_data, "cascades.gpkg"), append = FALSE)
sf::st_write(yell, here::here(dir_field_fig_data, "yell.gpkg"), append = FALSE)
sf::st_write(rmnp, here::here(dir_field_fig_data, "rmnp.gpkg"), append = FALSE)
sf::st_write(niwo, here::here(dir_field_fig_data, "niwo.gpkg"), append = FALSE)
sf::st_write(wref, here::here(dir_field_fig_data, "wref.gpkg"), append = FALSE)
sf::st_write(neon_sites_interest, here::here(dir_field_fig_data, "neon_sites_interest.gpkg"), append = FALSE)
sf::st_write(neon_sites_interest_points, here::here(dir_field_fig_data, "neon_sites_interest_points.gpkg"), append = FALSE)
sf::st_write(uas_plots, here::here(dir_field_fig_data, "uas_points.gpkg"), append = FALSE)
sf::st_write(all_poly_points, here::here(dir_field_fig_data, "field_poly_points.gpkg"), append = FALSE)





# ggplot2::ggplot() +
#   geom_sf(data = usa, aes(geometry = geometry)) +
#   geom_sf(data = neon_aop, aes(geometry = geometry))
# 
# 
# 
# ggplot2::ggplot() +
#   geom_sf(data = epa_l3 |> filter(NA_L3NAME == "Southern Rockies")) +
#   #geom_sf(data = usa, aes(geometry = geometry)) +
#   geom_sf(data = neon_aop, aes(geometry = geometry))




# Load, transform, prepare datasets ----
drone <- load_transform_sf("data/derived/uas_polygons_2_14_2025_analysis_ready.geojson", "uas", epsg) |>
  dplyr::mutate(extraction_group = plotID_clean)
aop_field <- load_transform_sf("data/derived/aop_polygons_2_14_2025_analysis_ready.geojson", "aop_field", epsg)  |>
  dplyr::mutate(extraction_group = aop_site)
aop_trees <- load_transform_sf("data/derived/ard_weinstein_trees.geojson", "aop_trees", epsg) |>
  dplyr::mutate(extraction_group = siteID,
         ecoregion = case_when(siteID == "YELL" ~ "MiddleRockies",
                               siteID == "RMNP" ~ "SouthernRockies",
                               siteID == "NIWO" ~ "SouthernRockies",
                               siteID == "WREF" ~ "Cascades",
                               TRUE ~ NA))


hist(drone$area_m, breaks = 2000)
hist(drone$area_m, breaks = 2000, xlim = c(0, 1))
hist(aop_field$area_m, breaks = 2000, xlim = c(0, 20))



d_shade <- drone |>
  group_by(shaded, cover_category) |>
  summarize(n = n())


