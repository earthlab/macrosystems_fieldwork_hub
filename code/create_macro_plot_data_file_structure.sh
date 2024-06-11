# Place this shell script in the directory where you want to create a UAS plot directory for Macrosystems, and then run the script

#!/bin/bash

# Get the directory path of the script
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Prompt the user for the folder name
read -p "Enter the plot abbreviation (e.g. SPR1): " plot_abv
read -p "Enter the flight date (e.g. 05-26-23): " plot_date
read -p "Enter the human-readable location (e.g. South Platte River): " location

# Create the folder
folder_name="${plot_abv}-${plot_date}"
folder_path="$script_dir/$folder_name"
mkdir "$folder_path"

#Create subfolders

folder_process="$folder_path/ImageryProcessing"
mkdir "$folder_process"

folder_package="$folder_path/TilePackages"
mkdir "$folder_package"

folder_raw="$folder_path/RawData"
mkdir "$folder_raw"

folder_meta="$folder_path/Metadata"
mkdir "$folder_meta"

folder_out="$folder_path/FinalOutputs"
mkdir "$folder_out"

folder_export="$folder_path/${folder_name}-ExportPackage"
mkdir "$folder_export"

folder_m300="$folder_raw/M300"
mkdir "$folder_m300"

folder_mini2="$folder_raw/Mini2"
mkdir "$folder_mini2"

folder_m300process="$folder_process/M300"
mkdir "$folder_m300process"

folder_mini2process="$folder_process/Mini2"
mkdir "$folder_mini2process"

folder_skyp="$folder_meta/SkyPhotos"
mkdir "$folder_skyp"

folder_sd1="$folder_m300/SD1"
mkdir "$folder_sd1"

folder_sd2="$folder_m300/SD2"
mkdir "$folder_sd2"

#Create metadate file
metadata_file="$folder_meta/Metadata.txt"
echo "This data is for Macrosystems plot $plot_abv at $location. All data is from $plot_date unless otherwise specified. Photos depict sky conditions at time of flight if non-standard. Detailed flight information, condition information, and GCP metadata is located in the "Drone Field Data Macrosystems" Google Sheet, located at https://docs.google.com/spreadsheets/d/1KahrdF1yzqLVi6hSEmn4hYBvZXWP1Uupj8Vozob_WtY.

Data for this plot should include:
-Multispectral photos from Matrice 300 in "M300/SD1" and "M300/SD2"
-RGB photos from Mini 2 in "Mini2"
-Sky photos showing conditions in "Meta/Skyphotos" if non-standard conditions
-A .txt file containing GPS information for GCPs, if GCPs were used. This dataset may include points from other plots.
-A .kml file used for drone mapping flights
-A folder containing a shapefile & zipped shapefile of the exact plot boundaries." > "$metadata_file"
