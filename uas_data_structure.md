# Macrosystems UAS Data Structure

To create the standardized Macrosystems UAS Data Structure, put the shell script [create_macro_plot_data_file_structure.sh](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/create_macro_plot_data_file_structure.sh) in the directory where you would like to create your plot subdirectory.

When you run this shell script, you will be asked for a few pieces of standardized information. From this user-supplied information, the script will generate the full structure outlined below.

```
C:.                      # Your root directory will be identified as follows: plot#-date (e.g. kremmling_2-06-05-24)
├───FinalOutputs         # This is where final output files should go (e.g. TIFs, LAZs, PDFs)
├───ImageryProcessing    # This is where Metashape processing documents and associated files should go (i.e. psx & auto-generated directories)
│   ├───M300
│   └───Mini2
├───Metadata             # This is where Metadata should go (plot shapefiles and kmls). It will have an auto-generated .txt file
│   └───SkyPhotos        # If you took photos documenting any non-ideal sky conditions at time of UAS flight, put them here
├───RawData              # This is where raw data files from UAS SD cards should go
│   ├───M300
│   │   ├───SD1
│   │   └───SD2
│   └───Mini2
├───showStructure1-today-ExportPackage          # This subdirectory should be filled with the final set of data that you want exported to CyVerse, as it will be pulled via script (usually FinalOutputs + Metadata)
└───TilePackages          # After creation, it is best practice to upload the tile packages created for digital mapping to this subdirectory
```
