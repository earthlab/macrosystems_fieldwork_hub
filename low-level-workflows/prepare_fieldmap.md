# Prepare an ArcGIS FieldMap for digital mapping in the field

## Upload your data to ArcGIS Online
1. Log in to ArcGIS Online: [https://www.arcgis.com/index.html]
   * Ideally you should be using the Earth Lab general ArcGIS Online account to ensure that all datasets will remain with Earth Lab rather than being attached to a personal account
2. Got to "Content"
3. Click on "New Item" at top left
4. Drag and drop your data to upload it
    * When asked how you would like to add the file, select the first option (to add the file AND create a hosted layer)
    * Click Next
    * Select the appropriate folder (i.e. YEAR_Macro)
    * Click save
5. Do step 4 for the following sets of data:
    * micaSense_falseColor tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
    * micaSense_realColor tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
    * RGB_highres tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
    * If not already uploaded to ArcGIS Online, the plot set for your area, which should be in .gpkg form (from plot generation scripts)
6. After each tile package uploads, set properties on the page pulled up:
     * Set visible range from World all the way to Room
     * Toggle Offline Mode so that it is ON
     * Click Save
7. After each plot set gpkg or shapefile uploads:
     * Go to the file's Visualization page and change the fill to be NULL and the boundary to be red. Change the visible range to be from Room -> World
     * Go to the file's Settings and scroll down to "Feature Layer (Hosted)". Check the checkbox for "Enable Sync" and save the settings. This is necessary for offline use.
         * IF YOU ARE GETTING AN ERROR that says that you are unable to save the change of settings after Enabling Sync, use this workaround:
             * In the hosted feature layer details, click "Publish" and then "Vector Tile layer"
             * Keep the tiling scheme defaults that come up and click "Next"
             * Add "_vTile" to the end of the file's name and then click "Save"
             * Go into the new vector tile layer's settings and make sure that Offline Use is toggled ON
             * You will now add this to your map instead of the hosted feature layer

  
## Create a new map with the data you want to use for field mapping
1. From your ArcGIS Online Home screen, click "Map" at top
2. At top left, click "Add" to add data
3. The layers you just uploaded and worked with should show up at the top of the default list. If they do not, use the search bar to find them.
4. Add all tile packages for the set of plots to the map
5. Add the plot set boundaries to the map, either the hosted feature layer or the hosted vector tile (if workaround was used above)
6. Add the field mapping polygon set of interest to the map. These feature layers already have the field map forms embedded in them.
   * macrosystems_uas_polygons for UAS mapping
   * aop_macrosystems_data for AOP mapping
7. Make sure that the visualization of all layers is as you want it
8. Make sure that the layer orders are correct:
   * _Top_
   * Field mapping polygon set
   * Plot boundaries
   * RGB high res packages
   * MicaSense realColor packages
   * MicaSense falseColor packages
   * _Bottom_
10. Save your map

## Create your FieldMap
1. Click the icon with 9 dots at upper right
2. Select "Field Maps Designer"
3. You should see the map that you just saved in the list of maps that comes up when the page loads. Click on it.
4. You should see a mapping form show up that has been embedded in the field polygon set.
    * If it is the first field day of the season, make any necessary changes (e.g. adding new names to the list of Collectors). Otherwise, it should be good to go already.
5. Click "Offline" in the bar at far left
     * Make sure that Offline is toggled ON at the top of the page
     * If it is NOT toggled on, you most likely have an error in one of your layers (should be listed below the toggle). Resolve these issues. You've most likely forgotten to turn on "Enable Sync" for a feature layer or "Offline use" for a tile package. Return and check to see if you can now toggle on "Offline"
  
## Enable OverZoom

  


Return to [UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md) or AOP Digital Mapping Workflow
