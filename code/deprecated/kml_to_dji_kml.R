library(here)
library(readr)
library(stringr)

#CHANGE THESE PARAMETERS
dir <- here::here('data', 'fieldwork', 'testing')
kmlFile <- 'r_test_kml.kml'

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


### ### ###
# A function to read in a KML file and turn it into a DJI-compatible KML, then export it ----
# dir = directory for output
# kmlFile = file path of KML to transform
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
  
  #Remove original kml write-out
  if(file.exists(fileNm)) {
    unlink(fileNm)
  } else {
    print("File does not exist")
  }
}

kml.to.dji.kml(here::here('data', 'fieldwork', 'plots', 'MRST'), "MRST2.kml")



  