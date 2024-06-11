# Georeference Imagery

Georeferencing (sometimes called rubbersheeting) is a useful approach for re-referencing a raster to a new spatial location, or for adding spatial information to an image such as a scan of a map.

Georeferencing can be completed in either QGIS or ArcGIS Pro. The workflow here is for ArcGIS Pro since the tile packages created immediately after this step need to be created in ArcGIS Pro.

1. Open the two sets of imagery that you want to georeference together
  * For Macrosystems, we want to georeference the RGB imagery TO the Micasense imagery, since this imagery was collected using an RTK system for corrections.
2. Click "Imagery" in the top toolbar
3. Select "Georeference" - the Georeferencing panel should open at the top
4. Click "Add Control Points" in the top panel. If this button is greyed out, it is likely because you have a projection mismatch between the layers you are working with. Reproject the layers and try again.
5. Click on your source imagery to set control points first, and then on your reference imagery. (You should see the words next to your cursor change after you click)
6. Click "Transformation" at top and then "2nd Order Polynomial". You will need at least 6 control points set.
7. Check to see how well the transformation has worked by toggling the georeferenced imagery on and off.
8. Once you are satisfied with your result, click 'Save as new'. Save your file with the same name as the original, but with "_georef" added to the end before the file extension. (e.g. ...MINI2_ORTHO_georef.tif)
9. Click export

Return to [UAS Digital Mapping workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md). You can now move on to exporting tile packages.
