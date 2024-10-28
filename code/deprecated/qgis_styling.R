 
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



library(here)
library(tidyverse)
csv <- read.csv(here::here('data/existing-vegetation-type/LF2020_EVT_220_CONUS/CSV_Data/LF20_EVT_220.csv'))
csv <- csv %>%
  mutate(across(everything(), ~replace(., is.na(.), "NA")))

create.qgis.style.for.paletted.raster.from.csv(styleData = csv, outputQmlPath = here::here('data/derived/test.qml'), valueColumn = "VALUE", labelColumn = "EVT_NAME", colorScheme = "RGB")
  

#crop EVT to just middle rockies
library(terra)
evt <- terra::rast(here::here('data/existing-vegetation-type/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif'))


