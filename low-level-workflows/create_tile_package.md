# Instructions for creating a tile package

Tile packages are necessary for using raster data in ArcGIS FieldMaps, which we use for our digital mapping workflow. Instructions for this step are below for both ArcGIS Pro and ArcMap.

## ArcGIS Pro
* Open orthomosaic(s) in ArcGIS Pro
  * Create a new “Map” project
  * Go to “Map” tab, “Add data” button, then “Add data”
  * Navigate to geotiff file, select it, then hit “OK”
* Clip to plot area if necessary (unnecessary for Macrosystems)
* Visualize as desired. For Macrosystems M300 imagery, see below:
  * M300 true color: R-G-B, 6-4-2 (~7cm)
  * M300 false color: NIR-R-G, 10-6-4 (~7cm)
  * Note that you will need to export a new tile package for each different visualization that you want, since tile packages are essentially a visual snapshot of the data.
* Remove default base maps from new project (Right click the layer, click “Remove”)
  * (Create Map Tile Package does not run with them on)
* Toggle off the layers you aren’t tiling yet
* Set Map description property (won’t run without it)
  * Right click on “Map” in contents pane
  * Metadata -> Properties -> Metadata -> Description (fill in this text box with something descriptive)
* Use tool: **Create Map Tile Package**
  * Set zoom level to appropriate scale [Zoom level to scale converter](https://developers.arcgis.com/documentation/mapping-apis-and-services/reference/zoom-levels-and-scale/#conversion-tool) (see "Tile Package Parameters" section) for more information on parameters)
* Save and export tile package
* Load tile package back into your document to confirm that it generated appropriately
* Repeat these steps for each layer you would like to be available in FieldMaps


## ArcMap
* Open orthomosaic
* Visualize as desired. For Macrosystems M300 imagery, see below:
  * M300 true color: R-G-B, 6-4-2 (~7cm)
  * M300 false color: NIR-R-G, 10-6-4 (~7cm)
  * Note that you will need to export a new tile package for each different visualization that you want, since tile packages are essentially a visual snapshot of the data.
* Add document description
  * File > Map Document Properties > Add Description 
* Toggle off the layers you aren’t tiling yet
* Save Map 
* Use: **Create Map Tile Package**
  * Select map document 
* Repeat these steps for each layer you would like to be available in FieldMaps

# Tile Package Parameters
[Details on tile package parameters can be found here](https://pro.arcgis.com/en/pro-app/latest/tool-reference/data-management/create-map-tile-package.htm).

* Tiling Format: Use JPEG format for high-resolution rasters, and PNG for anything that you need transparency in
* Extent: Default is usually fine, but check tile package after creation by loading it back in to double-check
* Compression quality: 75 seems to work fine for our purposes
* Set zoom level to appropriate scale [Zoom level to scale converter](https://developers.arcgis.com/documentation/mapping-apis-and-services/reference/zoom-levels-and-scale/#conversion-tool)
  * Macrosystems maximum scales:
    * UAS Imagery: 23
    * AOP imagery: 22
    * 30m raster: 17
  * Macrosystems minimum scales:
    * 0 (it’s sometimes convenient to have the lower scales to make it easy to find the imagery when zoomed out, e.g. if get lost on the map or if load while in a different location and it auto-zooms to your GPS location. However, if the large white tiles around your imagery bother you, you can increase this by a few scales)
    * 

Return to [UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md).
