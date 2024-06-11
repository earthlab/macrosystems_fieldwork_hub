# Macrosystems Fieldwork Hub
A repository for all things Macrosystems fieldwork related, including macrosystems workflows (e.g. digital mapping, UAS processing, plot creation)

# Important Links
* [Fieldwork GDrive](https://drive.google.com/drive/folders/1ARom7ANamjEfgzOd7agm1PTxPF1K1z9b?usp=drive_link)
* [AvenzaKMLs Folder](https://drive.google.com/drive/folders/164l4PtLY79svWSYn51ZN3AQwKh8EqROY?usp=drive_link)
* [Drone data spreadsheet (recording flights, weather, gcps)](https://docs.google.com/spreadsheets/d/1KahrdF1yzqLVi6hSEmn4hYBvZXWP1Uupj8Vozob_WtY/edit?usp=drive_link)
* [2024 Field Day Tracking & Data Processing](https://docs.google.com/spreadsheets/d/1HtjINMxrU8guyTSxz_OdsRKZ9KZ4zYN_TVUaIBOXzow/edit?usp=drive_link)
* [Earth Lab Drone Manual (including weather documentation protocol)](https://docs.google.com/document/d/1r_OoGRn0J6M--mA5OQc8BaterLIH4j6VHpvokBaC-9o/edit?usp=sharing)

# Complete List Of Workflows
This repository includes a number of workflows, both code-based and written. An overview of them is provided below. We recommend accessing lower-level workflows via links in the high level workflows.

## High Level Workflows
* [**uas_digital_mapping_workflow.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md) - This workflow outlines the entire process for generating spectral data samples from UAS for the Macrosystems project. It runs from identifying and creating plots through to usable data and references multiple lower-level workflows
* **aop_digital_mapping_workflow.md** - This workflow outlines the process for generating spectral data samples from NEON AOP data for the Macrosystems project. It runs from identifying high priority target areas through to usable data and references multiple lower-level workflows.

## Low Level Workflows
* [**create_tile_package.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/create_tile_package.md) - This workflow outlines how to create a tile package from a .tif file (e.g. from NEON AOP or UAS raster data)
* [**uas_data_structure.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/uas_data_structure.md) - This workflow outlines the standardized data structure used for Macrosystems UAS packages
* [**uas_field_protocols.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/uas_field_protocols.md) - This workflow outlines standard UAS protocols and parameters for the Macrosystems project
* [**metashape_workflow.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/metashape_workflow.md) - This workflow outlines the specific Metashape workflow used for Macrosystems data processing
* [**process_uas_data.md**](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/low-level-workflows/process_uas_data.md) - This workflow outlines how to process UAS data using Metashape on CU Boulder's research computing, how to extract key metadata, and how to transfer data back to local for tile package creation.
