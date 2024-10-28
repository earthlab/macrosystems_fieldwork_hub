// link to online script: https://code.earthengine.google.com/faa828e37dc8a084f4e1a7f9504fe4f7

// This script creates conservative versions of the 2022 Landfire EVT data
// by removing speckle. Tyler L. McIntosh, CU Boulder Earth Lab

var evt = ee.ImageCollection("projects/sat-io/open-datasets/landfire/vegetation/EVT"); // gee's EVT version is old; this is 2.3.0

print(evt)

// Get EVT data of interest
var evtConus = ee.Image(evt.filter(ee.Filter.eq('system:index', 'LC22_EVT_230')).first());

print(evtConus)


// // Define a square kernel with a radius of X pixels (1 = 3x3 window, 2 = 5x5 window)
// var squareKernel = ee.Kernel.square({radius: 2});

// // Calculate the mode of the 3x3 window for each pixel
// var mode = evtConus.reduceNeighborhood({
//   reducer: ee.Reducer.mode(),
//   kernel: squareKernel
// });

// // Create a mask where the focal mode equals the pixel value
// var mask = mode.eq(evtConus);

// // Apply the mask to the original band
// var evtConusConservative = evtConus.updateMask(mask);

function applyFocalModeWithMask(image, radius) {
  // Define a square kernel with a specified radius
  var squareKernel = ee.Kernel.square({radius: radius});

  // Calculate the mode of the window for each pixel
  var mode = image.reduceNeighborhood({
    reducer: ee.Reducer.mode(),
    kernel: squareKernel
  });

  // Create a mask where the focal mode equals the pixel value
  var mask = mode.eq(image);

  // Apply the mask to the original image
  var maskedImage = image.updateMask(mask);

  return maskedImage;
}


var evtConusConservative3 = applyFocalModeWithMask(evtConus, 1);
var evtConusConservative5 = applyFocalModeWithMask(evtConus, 2);



/////////////////////////////////
// Names of the 11 western US states
var westernStatesNames = [
  'Washington', 'Oregon', 'California', 'Idaho', 'Nevada', 'Montana',
  'Wyoming', 'Utah', 'Colorado', 'Arizona', 'New Mexico'
];

// Load US states feature collection
var states = ee.FeatureCollection('TIGER/2018/States');

// Filter the feature collection to include only the 11 western states
var westernStates = states.filter(ee.Filter.inList('NAME', westernStatesNames));
//////////////////////////////

var evtWestConservative3 = evtConusConservative3.clip(westernStates);
var evtWestConservative5 = evtConusConservative5.clip(westernStates);

// Add the result to the map (visualization purposes only)
Map.addLayer(evtWestConservative3, {min: 0, max: 3000}, 'focal 3');
Map.addLayer(evtWestConservative5, {min: 0, max: 3000}, 'focal 5');


Export.image.toDrive({
  image: evtWestConservative3.toFloat(), //cast to float to maintain masked NAs?? .toFloat()
  description: 'conservative_evt_west_3window',
  fileNamePrefix: 'conservative_evt_west_3window',
  folder: 'GEE_Exports',
  region: westernStates,
  scale: 30,
  crs: "EPSG:5070", // Albers equal area
  maxPixels: 1e13,
  fileFormat: 'GeoTIFF'
});

Export.image.toDrive({
  image: evtWestConservative5.toFloat(), //cast to float to maintain masked NAs?? .toFloat()
  description: 'conservative_evt_west_5window',
  fileNamePrefix: 'conservative_evt_west_5window',
  folder: 'GEE_Exports',
  region: westernStates,
  scale: 30,
  crs: "EPSG:5070", // Albers equal area
  maxPixels: 1e13,
  fileFormat: 'GeoTIFF'
});

