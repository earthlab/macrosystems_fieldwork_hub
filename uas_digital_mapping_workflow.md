# UAS Digital Mapping Workflow
This document provides a high level view of how to perform the Macrosystems UAS digital mapping workflow. It links out to other more low-level workflows contained in this repository. This workflow is similar to that for NEON AOP digital mapping, but with some slight differences.

1. Establish plot locations
2. Generate plot files
3. Fly UAS plot
4. Create UAS data file structure and transfer raw data to PetaLibrary
5. Process UAS data via Metashape on CU Research Computing and transfer back to local
6. Generate tile packages: (use this [Tile Package Creation workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/create_tile_package.md))
   * M300 Real Color
   * M300 False Color
   * Mini2
7. Create an ArcGIS Field Map document with the following data:
   * All UAS tile packages
   * The overarching macrosystems_uas_polygons feature layer
     * Ensure that the fieldmaps form embedded in the feature layer is up to date! The first time each summer you will need to add in the names of new field staff.
   * Actionable plot boundary
8. Return to plot to perform digital mapping in the field
9. Sync polygon data on return and perform QA/QC
