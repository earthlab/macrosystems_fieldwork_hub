# Macrosystems UAS Field Protocols

Macrosystems UAS plots are flown with two units:
* A UAS capable of carrying a payload of a MicaSense RedEdge 10 band camera (multispectral), e.g. a Matrice 300 RTK or similar
* A UAS with an integrated high resolution RGB camera, e.g. a DJI Mini2 or Phantom 4 Pro or similar

These two data sources help with interpreting the imagery for digital mapping in the field. The MicaSense imagery is the actual data from which spectral signatures for each sample are extracted, while the RGB imagery is solely for interpretive assistance.

Instructions for flight planning and field day preparation, as well as flight parameters, can be found in the flight planning and UAS field day preparation workflow. {ADD THIS}

This workflow assumes that you have already performed all preparation and planning and are ready to enter the field to fly. This workflow is best with three people. A UAS field day gear checklist is available here.

UAS flights for Macrosystems should take place within 3 hours of solar noon if you are trying to complete >3 plots, or within 2 hours of solar noon if you only have a few plots to complete.

# Prior to entering field
1. Collect all field gear necessary (checklist available here)
2. Distribute radios
3. Brush off field boots with stiff bristled brush to avoid transmission of invasives between sites

# At the field site
1. Navigate to plot using plot KML loaded to Avenza as a guide on a GPS-enabled phone or tablet
2. Walk out to examine plot
   * Does the plot appear as expected? Is there a variety of cover, and does it more or less match what is expected from EVT analysis?
   * How homogenous is the plot? If it is relatively homogenous and lacking in specific identifying features, GCPs may be placed in a triangle across the plot to assist in digital mapping and orthomosaic georeferencing. Process for each GCP:
       * Place GCP tarp and ensure it is pulled taught with four stakes
       * Place flags at each corner of the tarp. It is recommended to place a flag of a different color at the corner that has the GCP tarp number, which can help with orienting in relation to the high resolution RGB imagery.
       * Recommended: Add a waypoint on Avenza to mark where the GCP is located. This is useful for going back to pull them up.
3. Set up geolocation protocol (either RTK - recommended, guidance below; or use GCPs)
   * Set up RTK unit and start station
   * IF DJI RTK2, ensure that the far-right light is blinking five times and then pausing
   * The RTK will cycle until it has enough satellite connections (middle light will go from red to yellow to green)
5. Get out the fire extinguisher for easy access
6. Set up RGB UAS while starting MicaSense UAS setup (see below)
7. Once RGB UAS is ready, **perform flight** while someone continues setting up MicaSense UAS (see below)
8. Set up the MicaSense UAS. Your team will likely have a detailed UAS checklist for your specific unit. Below is a general flow
   * Unfold and set up drone
   * Attach MicaSense camera and remove camera caps
       * Ensure that SD cards are in the MicaSense camera
   * Turn on drone
   * BEFORE connecting to controller, connect to the rededge wifi signal on a cell phone or laptop
   * Open the MicaSense camera app: http://192.168.10.254/
       * Check that satellites are connected, there is GPS signal (triggering may fail if no signal), and that there is space on the SD cards
       * Go to Advanced Settings and make sure that settings match with your flight plan:
           * FILL THIS IN
       * Click on ___________ and perform the drone dance 
   * Take a photo of the calibration panel [ADD MORE DETAILS HERE]
9. Record information on the [Drone Data Spreadsheet](https://docs.google.com/spreadsheets/d/1KahrdF1yzqLVi6hSEmn4hYBvZXWP1Uupj8Vozob_WtY/edit?usp=drive_link); this should include your location, flight information, and sky conditions documentation
   * If sky conditions are non-standard (i.e not perfectly clear or perfectly overcast), take 5 photos with the 180-degree fish-eye lens: one directly up, and one to each cardinal direction.
11. Double check flight parameters and ensure that the RTK is connected
12. Begin MicaSense UAS flight
   * If you are flying with scattered clouds and a cloud approaches the sun, PAUSE flight until after the cloud has passed the sun and moved away. Make a note of htis irregularity on the Drone Data Spreadsheet.
12. Finish flight and land safely
13. Take a second photo of the calibration panel [ADD MORE DETAILS HERE]
14. Power down the UAS
15. Remove the SD cards from the MicaSense camera and insert them into your computer. Ensure that:
    * There are new photos with today's date
    * That there is a full set of photos (each SD card should contain ~3,000 files. If you have significantly fewer, this is a red flag that you may have had a sensor triggering error and do not have a complete set of images.
16. Fold up UAS
17. Shut down RTK unit
18. Pull up GCP tarps and stakes (if placed), but LEAVE flags. These will be removed upon return for digital mapping. (Use waypoints recorded on Avenza if you placed them while out)

Return to [UAS Digital Mapping Workflow](https://github.com/earthlab/macrosystems_fieldwork_hub/blob/main/uas_digital_mapping_workflow.md).
