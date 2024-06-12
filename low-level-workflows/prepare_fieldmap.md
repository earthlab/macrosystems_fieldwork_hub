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
     * Go to the file's Settings and scroll down to "Feature Layer (Hosted)". Check the checkbox for "Enable Sync" and save the settings. This is necessary for offline use.
     * Go to the file's Visualization page and change the fill to be NULL and the boundary to be red. Change the visible range to be from Room -> World
  
## Create a new map with the data you want to use for field mapping
1. 

Return to [UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md) or AOP Digital Mapping Workflow
