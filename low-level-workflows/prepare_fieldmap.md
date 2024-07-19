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
5. Do step 4 for all sets of data that you need
    * UAS Field Mapping:
      * micaSense_falseColor tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
      * micaSense_realColor tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
      * RGB_highres tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
      * If not already uploaded to ArcGIS Online, the plot set for your area, which should be in .gpkg form (from plot generation scripts)
    * AOP Field Mapping:
      * Your AOP fieldmap group tile package (from [Create Tile Package workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
      * The EVT layer tile package for your NEON site if it isn't already loaded onto ArcGIS Online
7. After each tile package uploads, set properties on the page pulled up:
     * Set visible range from World all the way to Room
     * Toggle Offline Mode so that it is ON
     * Click Save
8. After each plot set gpkg or shapefile uploads:
     * Go to the file's Visualization page and change the fill to be NULL and the boundary to be red. Make the plot boundary line 4pt size. Change the visible range to be from Room -> World. At upper right, make sure to click 'Save layer' before going back out.
     * Go to the file's Settings. Click the check box under 'Public Data Collection'. Now scroll down to "Feature Layer (Hosted)". Check the checkbox for "Enable Sync" and save the settings. This is necessary for offline use.
         * IF YOU ARE GETTING AN ERROR that says that you are unable to save the change of settings after Enabling Sync, use this workaround:
             * In the hosted feature layer details, click "Publish" and then "Vector Tile layer"
             * Keep the tiling scheme defaults that come up and click "Next"
             * Add "_vTile" to the end of the file's name and then click "Save"
             * Go into the new vector tile layer's settings and make sure that Offline Use is toggled ON
             * You will now add this to your map instead of the hosted feature layer

  
## Create a new map with the data you want to use for field mapping
1. From your ArcGIS Online Home screen, click "Map" at top
2. At top left, click "Add" to add data
3. Add your data to the map. The layers you just uploaded and worked with should show up at the top of the default list. If they do not, use the search bar to find them.
   * UAS Field Mapping:
     * Add all tile packages for the set of plots to the map
     * Add the plot set boundaries to the map, either the hosted feature layer or the hosted vector tile (if workaround was used above)
   * AOP Field Mapping:
     * Add the fieldmap group tile package you are working with
     * Add the EVT tile package for the NEON site
7. Add the field mapping polygon set of interest to the map. These feature layers already have the field map forms embedded in them.
   * UAS Mapping: macrosystems_uas_polygons
   * AOP Mapping: aop_macrosystems_data
8. Make sure that the visualization of all layers is as you want it
9. Make sure that the layer orders are correct:
   * UAS Mapping
     * _Top_
     * Field mapping polygon set
     * Plot boundaries
     * RGB high res packages
     * MicaSense realColor packages
     * MicaSense falseColor packages
     * _Bottom_
   * AOP Mapping
     * _Top_
     * Field mapping polygon set
     * EVT tile package
     * AOP imates fieldmap group tile package
     * _Bottom_
11. Save your map

## Create your FieldMap
1. Click the icon with 9 dots at upper right
2. Select "Field Maps Designer"
3. You should see the map that you just saved in the list of maps that comes up when the page loads. Click on it.
4. You should see a mapping form show up that has been embedded in the field polygon set.
    * If it is the first field day of the season, make any necessary changes (e.g. adding new names to the list of Collectors). Otherwise, it should be good to go already.
5. Click "Offline" in the bar at far left
     * Make sure that Offline is toggled ON at the top of the page
     * If it is NOT toggled on, you most likely have an error in one of your layers (should be listed below the toggle). Resolve these issues. You've most likely forgotten to turn on "Enable Sync" for a feature layer or "Offline use" for a tile package. Return and check to see if you can now toggle on "Offline"
6. Go to "App Settings" in the bar at far left
     * Scroll down to "Collecting New Features"
     * Select "No changes to the map"
     * Under "Manual Location", make sure that "Allow manual location" is selected
7. Save your changes!! Click the floppy disk save icon at upper right (next to the circle arror, right underneath the account name)
  
## Enable OverZoom

[USE THIS OTHER WORKFLOW FOR NOW] - HAS IMAGES
https://docs.google.com/document/d/1ff1AYpDaB1ByZPJ3qEwg4VkytPw-PHX_sW6oOjlBhvo/edit

## Download for offline use
1. Open ArcGIS FieldMaps on the phone or tablet that you would like to use for digital mapping
2. Refresh to ensure that all data is updated
3. Click the three dots next to the map you would like to take offline -> “Add offline area”
4. Change level of detail as appropriate for purpose / rasters
   * “Rooms” will allow about 3x5 AOP tiles to be downloaded
   * “Room” will allow a bit less than one AOP tile to be downloaded
   * “Room” is appropriate for all drone imagery
5. Zoom to your minimum-necessary area (this will impact the download size)
6. Download area
7. Once downloaded, click the three dots next to the downloaded area to change the name
8. Repeat steps 3-8 for each plot or area of the field map that you would like to make accessible offline




Return to **[UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md)** or **[AOP Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/aop_digital_mapping_workflow.md)**
