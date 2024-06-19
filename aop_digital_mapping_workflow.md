# AOP Digital Mapping Workflow
This document provides a high level view of how to perform the Macrosystems AOP digital mapping workflow. It links out to other more low-level workflows contained in this repository. This workflow is similar to that for UAS digital mapping, but with some slight differences.

Note that this workflow is designed to allow digital mapping on ArcGIS FieldMaps. While there is an open source alternative (QField), this is newer and not as stable as of when this workflow was initiated in 2022-2023.

1. Identify high priority / variability NEON AOP areas through an iterative randomized process
2. Identify and record AOP tile names and field map groups
    * [Record YELL NEON Tiles to download for summer 2024 on this spreadsheet](https://docs.google.com/spreadsheets/d/1l3mCbow1SDMhjCer7tvt09IdgSv64sP3qFBSAZ4u7ko/edit#gid=0) 
4. Access and download AOP tiles
    * Visit the [NEON Data Explorer and Download Portal](https://data.neonscience.org/data-products/explore)
    * Find the "High-resolution orthorectified camera imagery mosaic" dataset, by either filtering or using the search bar at left.
    * Click the blue "Download Data" button on the dataset
    * Select your NEON site of interest from the rows of sites. Each NEON site has a 4-letter code associated with it. The codes for the Macrosystems-associated sites are:
      * YELL: Yellowstone (WY/MT)
      * NIWO: Niwot Ridge (CO)
      * WREF: Wind River Experimental Forest (OR)
    * Use the date range boxes to restrict the temporal window of your data selection to only include the **most recent** AOP dataset (marked by the blue or hashed blue boxes).
      * Note that the hashed icons indicate provisional data. This means that the data have been processed by NEON and should be the same as the final released product, but they are waiting for feedback from the user community before officially releasing it. Our digital mapping workflow relies on close temporal sequencing between imagery and mapping in the field, so we use provisional data when necessary.
    * Click 'Next'
    * If the most recent data is provisional, you will need to click 'Include' to include provisional data.
    * Select the AOP tiles of interest. Note that you can use the search bar in the 'Name' column to find specific tiles.
         * [This spreadsheet contains YELL NEON Tiles to download for summer 2024](https://docs.google.com/spreadsheets/d/1l3mCbow1SDMhjCer7tvt09IdgSv64sP3qFBSAZ4u7ko/edit#gid=0)
    * Click 'Next'
    * You should be able to exclude documentation if the only use case is digital mapping.
    * Agree to the usage and citation policies
    * Click the blue 'Download Data' button
10. Generate tile packages for each field map group: (use this [Tile Package Creation workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
11. Create an ArcGIS Field Map document (see [Preparing a fieldmap workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/prepare_fieldmap.md)) with the following data:
     * Tile package for field map group
     * The overarching aop_macrosystems_data feature layer
     * Ensure that the fieldmaps form embedded in the feature layer is up to date! The first time each summer you will need to add in the names of new field staff.
     * EVT tile package
11. Perform digital mapping in the field
12. Sync polygon data on return and perform QA/QC
