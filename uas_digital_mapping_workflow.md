# UAS Digital Mapping Workflow
This document provides a high level view of how to perform the Macrosystems UAS digital mapping workflow. It links out to other more low-level workflows contained in this repository. This workflow is similar to that for NEON AOP digital mapping, but with some slight differences.

1. Establish plot locations
2. Generate plot files
3. Fly UAS plot (see [UAS Field Protocols](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_field_protocols.md))
   * UAS with MicaSense RedEdge Sensor attached (e.g. Matrice 300 or similar)
   * UAS with high-resolution RGB sensor (e.g. Phantom 4 Pro or DJI Mini2)
5. Create UAS data file structure and transfer raw data to PetaLibrary (see [UAS Data Structure](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_data_structure.md))
6. Process UAS data via Metashape on CU Research Computing and transfer back to local
7. Generate tile packages: (use this [Tile Package Creation workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/create_tile_package.md))
   * M300 Real Color
   * M300 False Color
   * Mini2
8. Create an ArcGIS Field Map document with the following data:
   * All UAS tile packages
   * The overarching macrosystems_uas_polygons feature layer
     * Ensure that the fieldmaps form embedded in the feature layer is up to date! The first time each summer you will need to add in the names of new field staff.
   * Actionable plot boundary
9. Return to plot to perform digital mapping in the field
10. Sync polygon data on return and perform QA/QC
