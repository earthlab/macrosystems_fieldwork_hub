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
* Remove default base maps from new project (Right click the layer, click “Remove”)
  * (Create Map Tile Package does not run with them on)
* Set Map description property (won’t run without it)
  * Right click on “Map” in contents pane
  * Metadata -> Properties -> Metadata -> Description (fill in this text box with something descriptive)
* Use tool: **Create Map Tile Package**
  * Set zoom level to appropriate scale [Zoom level to scale converter](https://developers.arcgis.com/documentation/mapping-apis-and-services/reference/zoom-levels-and-scale/#conversion-tool) (see "Tile Package Parameters" section) for more information on parameters)
* Save and export tile package

ArcMap:
Open orthomosaic
Visualize as desired
E.g.
RGB
M300 true color: R-G-B, 6-4-2 (~7cm)
M300 false color: NIR-R-G, 10-6-4 (~7cm)
File > Map Document Properties > Add Description 
Toggle off the layers you aren’t tiling yet
Save Map 
Use: Create Map Tile Package 
Select map document 
Parameter Details
Tiling format: JPEG high-resolution imagery, PNG for anything that you need transparency in
Extent: Whatever is appropriate
Compression quality: 75
Set zoom levels to appropriate scale Zoom level to scale converter
Maximum scale: 23 for UAS imagery, 22 for AOP, 17 for 30m imagery
Minimum scale: 0 (it’s good to have the lower scales to make it easy to find the imagery when zoomed out, e.g. if get lost on the map or if load while in a different location and it auto-zooms to your GPS location)
Repeat steps A - F for each layer you would like to be available in FieldMaps
