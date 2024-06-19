# Instructions for creating a tile package

Tile packages are necessary for using raster data in ArcGIS FieldMaps, which we use for our digital mapping workflow. Instructions for this step are below for both ArcGIS Pro and ArcMap (at very bottom). This same process works for creating tile packages regardless of what type of data is being tiled. The only change will be in the tile package parameters (after ArcGIS Pro instructions).

## ArcGIS Pro
* Open orthomosaic(s) in ArcGIS Pro
  * Create a new “Map” project
  * Go to “Map” tab, “Add data” button, then “Add data”
  * Navigate to geotiff file, select it, then hit “OK”
    * If you are working with AOP data, load all of the TIF files in the fieldmap group listed
      * ([See this spreadsheet for YELL summer 2024 tiles](https://docs.google.com/spreadsheets/d/1l3mCbow1SDMhjCer7tvt09IdgSv64sP3qFBSAZ4u7ko/edit#gid=0))
* Clip to plot area if necessary (unnecessary for Macrosystems)
* Visualize as desired.
  * AOP RGB imagery: use default visualization
  * Macrosystems MicaSense imagery:
    * MicaSense true color: Use bands R-G-B, numbered 6-4-2
    * MicaSense false color: Use bands NIR-R-G, numbered 10-6-4
  * High resolution RGB UAS imagery: use default visualization
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
  * Save the tile packages with the following naming conventions:
    * UAS Data
      * MicaSense false color: [plotNameAndNumber]_MicaSense_falseColor_tile_package
      * MicaSense real color: [plotNameAndNumber]_MicaSense_realColor_tile_package
      * High resolution RGB: [plotNameAndNumber]_RGB_highres_tile_package
    * AOP Data
      * [AOPCode]_[fieldmapGroup]_AOP_rgb_tile_package
      * (e.g. YELL_riverNorth1_AOP_rgb_tilePackage)
* Load tile package back into your document to confirm that it generated appropriately
* Repeat these steps for each layer you would like to be available in FieldMaps


**Return to [UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md).** OR **Return to [AOP Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/aop_digital_mapping_workflow.md).**


## Tile Package Parameters
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
   





## ArcMap instructions if using ArcMap
* Open orthomosaic
* Visualize as desired. For Macrosystems MicaSense imagery, see below:
  * MicaSense true color: R-G-B, 6-4-2 (~7cm)
  * MicaSense false color: NIR-R-G, 10-6-4 (~7cm)
  * Note that you will need to export a new tile package for each different visualization that you want, since tile packages are essentially a visual snapshot of the data.
* Add document description
  * File > Map Document Properties > Add Description 
* Toggle off the layers you aren’t tiling yet
* Save Map 
* Use: **Create Map Tile Package**
  * Select map document
* Save and export tile package
  * Save the tile packages with the following naming conventions:
    * plotNameAndNumber_MicaSense_falseColor_tile_package
    * plotNameAndNumber_MicaSense_realColor_tile_package
    * plotNameAndNumber_RGB_highres_tile_package
* Repeat these steps for each layer you would like to be available in FieldMaps
