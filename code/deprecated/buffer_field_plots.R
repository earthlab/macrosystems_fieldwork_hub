#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(terra)
library(sf)
library(here)
library(tidyverse)


#Function to write a shapefile to a new, file-specific directory and add a zipped version
#    shp = the sf file to write to shapefile
#    location = path of directory to create the new file-specific subdirectory in
#    filename = name of file WITHOUT .shp
#    zipOnly = TRUE / FALSE, should the original (unzipped) files be removed?

# Example use:
# st_write_shp(shp = prepped_for_parks_etal,
#              location = here("data/derived"),
#              filename = "career_lba_for_parks_v1",
#              zipOnly = TRUE)
st_write_shp <- function(shp, location, filename, zipOnly) {
  
  #Check for required packages and install if not installed, then load
  list.of.packages <- c("zip","sf","here")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(zip)
  library(sf)
  library(here)
  

   #Create subdirectory
   outDir <- here::here(location, filename)
   if (!dir.exists(outDir)){
     dir.create(outDir)
   }

  
  
  #Write file
  sf::st_write(shp,
               here::here(outDir, paste(filename, ".shp", sep = "")),
               append = FALSE) #overwrite
  
  #Get all shapefile components
  allShpNms <- list.files(here::here(outDir),
                          pattern = paste(filename, "*", sep = ""),
                          full.names = TRUE)
  
  #Zip together
  zip::zip(zipfile = here(outDir, paste(filename, ".zip", sep="")),
           files = allShpNms,
           mode = "cherry-pick")
  
  
  #Remove raw files if desired
  if(zipOnly == TRUE) {
    file.copy(here(outDir, paste(filename, ".zip", sep="")), here(location, paste(filename, ".zip", sep="")))
    unlink(here(outDir), recursive = TRUE)          
  }
  
}


### ### ###
# A function to read in a KML file and turn it into a DJI-compatible KML, then export it ----
kml.to.dji.kml <- function(dir, kmlFile) {
  #Read KML file as raw txt
  kml <- readr::read_file(here::here(dir,kmlFile))
  
  #Get the coordinates from the original KML file
  coords <- kml %>% 
    stringr::str_match("<coordinates>\\s*(.*?)\\s*</coordinates>")
  coords <- coords[,2]
  
  #Add elevation to the original coordinates
  newCoords <- coords %>%
    gsub(" ", ",0 ", .) %>%
    paste0(., ",0", sep="")
  
  #Get name from original KML file
  nm <- kml %>%
    stringr::str_match("<name>\\s*(.*?)\\s*</name>")
  nm <- nm[,2]
  
  #Combine all text with the correctly formatted KML file text from a Google Earth Pro-created KML file
  newKMLtxt <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n<Document>\n\t<name>", nm, ".kml</name>\n\t<StyleMap id=\"m_ylw-pushpin\">\n\t\t<Pair>\n\t\t\t<key>normal</key>\n\t\t\t<styleUrl>#s_ylw-pushpin</styleUrl>\n\t\t</Pair>\n\t\t<Pair>\n\t\t\t<key>highlight</key>\n\t\t\t<styleUrl>#s_ylw-pushpin_hl</styleUrl>\n\t\t</Pair>\n\t</StyleMap>\n\t<Style id=\"s_ylw-pushpin\">\n\t\t<IconStyle>\n\t\t\t<scale>1.1</scale>\n\t\t\t<Icon>\n\t\t\t\t<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>\n\t\t\t</Icon>\n\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>\n\t\t</IconStyle>\n\t</Style>\n\t<Style id=\"s_ylw-pushpin_hl\">\n\t\t<IconStyle>\n\t\t\t<scale>1.3</scale>\n\t\t\t<Icon>\n\t\t\t\t<href>http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png</href>\n\t\t\t</Icon>\n\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>\n\t\t</IconStyle>\n\t</Style>\n\t<Placemark>\n\t\t<name>", nm, "</name>\n\t\t<styleUrl>#m_ylw-pushpin</styleUrl>\n\t\t<Polygon>\n\t\t\t<tessellate>1</tessellate>\n\t\t\t<outerBoundaryIs>\n\t\t\t\t<LinearRing>\n\t\t\t\t\t<coordinates>\n\t\t\t\t\t\t", newCoords, " \n\t\t\t\t\t</coordinates>\n\t\t\t\t</LinearRing>\n\t\t\t</outerBoundaryIs>\n\t\t</Polygon>\n\t</Placemark>\n</Document>\n</kml>\n", sep = "")
  
  #Sink to new kml file
  newFile <- paste(substr(kmlFile, 1, nchar(kmlFile)-4), "_for_dji.kml", sep="")
  sink(here::here(dir, newFile))
  cat(newKMLtxt)
  sink()
}

### ### ###
#The above, but as an EXPORT function
#This function takes in an sf object, exports it as a normal .kml, reads it back in, exports it as a dji.kml file, and deletes the original .kml file
#It is useful for keeping directories clean if you only need dji.kml files
#This function will download and load the 'sf' package if it is not already installed
#This function requires the function above (kml.to.dji.kml)
#sfObj is an sf object to write out, fileNm is the name of the file (e.g. mykml.kml), and outDir is the export directory (e.g.here::here('my', 'directory'))
write.dji.kml <- function(sfObj, fileNm, outDir) {
  #Check the required libraries and download if needed
  list.of.packages <- c("sf")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(sf)
  
  #Write as normal kml
  sf::st_write(sfObj, here(outDir, fileNm), append = FALSE)
  
  #Use kml.to.dji.kml function
  kml.to.dji.kml(outDir, fileNm)
  print(fileNm)
  
  #Remove original kml write-out
  if(file.exists(fileNm)) {
    unlink(here(outDir, fileNm))
  } else {
    print("File does not exist")
  }
}


#Load plots
plots <- sf::st_read(here::here('data', 'fieldwork', 'aop_underflight', 'aop_underflight_areas_2023.shp'))

exportLocation = here('data', 'fieldwork', 'aop_underflight')

#For each plot, export as .kml for UAS
for(i in 1:nrow(plots)) {
  
  #Write plot to shp
  plot <- plots[i,]
  
  #Write plot to kml
  kmlnmP <- gsub(" ", "", paste(plot$Code, ".kml", sep = ""))
  print(kmlnmP)

  write.dji.kml(plot, kmlnmP, exportLocation)
}



### DEPRECATED


# #For each plot, export as its own shapefile and zip it, and then buffer by 30 ft and export as .kml for UAS
# for(i in 1:nrow(plots)) {
#   
#   #Write plot to shp
#   plot <- plots[i,]
#   
#   shpnm <- gsub(" ", "", paste(plot$Code, sep = ""))
#   print(shpnm)
#   st_write_shp(shp = plot, 
#                location = exportLocation, 
#                filename = shpnm, 
#                zipOnly = FALSE)
#   
#   #Write plot to kml
#   kmlnmP <- gsub(" ", "", paste(plot$Code, ".kml", sep = ""))
#   print(kmlnmP)
#   st_write(plot, here(exportLocation, plot$Code, kmlnmP), append = FALSE)
#   
#   #Buffer 30ft
#   buffed <- plot %>% sf::st_buffer(9.144) #30ft = 9.144m
#   
#   #Write 30ft buffer to shp
#   buffShpnm <- gsub(" ", "", paste(plot$Code, "_30ftbuff", sep = ""))
#   print(buffShpnm)
#   st_write_shp(shp = buffed, 
#                location = here(exportLocation, shpnm), 
#                filename = buffShpnm, 
#                zipOnly = TRUE)
#   
#   #Write 30ft buffer to kml
#   kmlnm <- gsub(" ", "", paste(plot$Code, "_30ftbuff.kml", sep = ""))
#   print(kmlnm)
#   st_write(buffed, here(exportLocation, plot$Code, kmlnm), append = FALSE)
#   
#   #Buffer 60ft
#   buffed2 <- plot %>% sf::st_buffer(18.288) #30ft = 9.144m
#   
#   #Write 60ft buffer to shp
#   buff2Shpnm <- gsub(" ", "", paste(plot$Code, "_60ftbuff", sep = ""))
#   print(buff2Shpnm)
#   st_write_shp(shp = buffed2, 
#                location = here(exportLocation, shpnm), 
#                filename = buff2Shpnm, 
#                zipOnly = TRUE)
#   
#   #Write 60ft buffer to kml
#   kmlnm2 <- gsub(" ", "", paste(plot$Code, "_60ftbuff.kml", sep = ""))
#   print(kmlnm2)
#   st_write(buffed2, here(exportLocation, plot$Code, kmlnm2), append = FALSE)
# }

