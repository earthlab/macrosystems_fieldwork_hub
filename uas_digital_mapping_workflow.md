# UAS Digital Mapping Workflow
This document provides a high level view of how to perform the Macrosystems UAS digital mapping workflow. It links out to other more low-level workflows contained in this repository. This workflow is similar to that for NEON AOP digital mapping, but with some slight differences.

Note that this workflow is designed to allow digital mapping on ArcGIS FieldMaps. While there is an open source alternative (QField), this is newer and not as stable as of when this workflow was initiated in 2022-2023.

1. Establish plot locations
2. Generate plot files
3. Prepare for field and perform flight planning
4. Go into the field and fly UAS plot (see [UAS Field Protocols](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/uas_field_protocols.md))
   * UAS with MicaSense RedEdge Sensor attached (e.g. Matrice 300 or similar)
   * UAS with high-resolution RGB sensor (e.g. Phantom 4 Pro or DJI Mini2)
5. Create UAS data file structure and transfer raw data to PetaLibrary (see [UAS Data Structure](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/uas_data_structure.md))
6. Process UAS data via Metashape on CU Research Computing and transfer back to local (see [workflow for processing UAS data](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/process_uas_data.md))
7. Visualize the multispectral MicaSense Imagery in two different ways: Real Color & False Color
   * To do so, load the imagery in ArcGIS Pro and then copy the imagery so that you have a second layer.
   * Right click on the imagery and then click 'Symbology' from the menu
   * Change the Red-Green-Blue bands
     * Real Color: Red = Band 6, Green = Band 4, Blue = Band 2
     * False Color: Red = Band 10, Green = Band 6, Blue = Band 4
9. Georeference the RGB imagery to the MicaSense imagery, which should be more accurate since it was collected using RTK corrections (see [Georeferencing Imagery workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/georeference_imagery)).
10. Generate tile packages: (use this [Tile Package Creation workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
   * M300 Real Color
   * M300 False Color
   * Mini2
11. Create an ArcGIS Field Map document (see [Preparing a fieldmap workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/prepare_fieldmap.md)) with the following data:
   * All UAS tile packages
   * The overarching macrosystems_uas_polygons feature layer
     * Ensure that the fieldmaps form embedded in the feature layer is up to date! The first time each summer you will need to add in the names of new field staff.
   * Actionable plot boundary
11. Return to plot to perform digital mapping in the field
12. Sync polygon data on return and perform QA/QC
