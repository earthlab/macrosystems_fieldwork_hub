# AOP Digital Mapping Workflow
This document provides a high level view of how to perform the Macrosystems AOP digital mapping workflow. It links out to other more low-level workflows contained in this repository. This workflow is similar to that for UAS digital mapping, but with some slight differences.

Note that this workflow is designed to allow digital mapping on ArcGIS FieldMaps. While there is an open source alternative (QField), this is newer and not as stable as of when this workflow was initiated in 2022-2023.

1. Identify high priority / variability NEON AOP areas through an iterative randomized process
2. Identify and record AOP tile names and field map groups
3. Access and download AOP tiles
10. Generate tile packages for each field map group: (use this [Tile Package Creation workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md))
11. Create an ArcGIS Field Map document (see [Preparing a fieldmap workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/prepare_fieldmap.md)) with the following data:
     * Tile package for field map group
     * The overarching aop_macrosystems_data feature layer
     * Ensure that the fieldmaps form embedded in the feature layer is up to date! The first time each summer you will need to add in the names of new field staff.
     * EVT tile package
11. Perform digital mapping in the field
12. Sync polygon data on return and perform QA/QC
