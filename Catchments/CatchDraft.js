var geometry = ee.Geometry.Point([-121.00196609379975,39.26830373600965]);
var hydro = require('users/gena/packages:hydro')

// get catchments (all of level 6 and given an outlet)
var catchments = hydro.getCatchments({level: 6})
var catchmentsSelected = hydro.getCatchments({outlet: geometry, level: 6})

Map.addLayer(catchments.style({color: 'ffffff55', width: 1, fillColor: '00000088'}), {}, 'catchments (all)')
Map.addLayer(catchmentsSelected.style({color: 'ffff0055', width: 1, fillColor: 'ffff0088'}), {}, 'catchments (selected)')


// add rivers (all)
var rivers = hydro.getRivers()
Map.addLayer(rivers.filter(ee.Filter.gt('UP_CELLS', 50000)).style({color: '00ffff', width: 1 }), {}, 'rivers (th > 50000)', true, 0.5)

// add rivers(selected catchments only)
var rivers = hydro.getRivers({catchments: catchmentsSelected})

Map.addLayer(rivers.filter(ee.Filter.gt('UP_CELLS', 10000)).style({color: '00ffff', width: 2 }), {}, 'rivers (selected, th > 10000)')
Map.addLayer(rivers.filter(ee.Filter.gt('UP_CELLS', 1000)).style({color: '00ffff', width: 1 }), {}, 'rivers (selected, th > 1000)')

var rivers = ee.FeatureCollection('users/gena/HydroEngine/riv_15s_lev06')

print(rivers.first())
print(catchmentsSelected.first())
