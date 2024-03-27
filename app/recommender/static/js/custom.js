// create map with attributions
const map = L.map('map').setView([-0.5, 37.5], 9)

L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 19,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);

const search = new GeoSearch.GeoSearchControl({
    provider: new GeoSearch.OpenStreetMapProvider(),
    style: 'bar',
  });
map.addControl(search); //add search control

// add polygon of soil region
const polygon = L.polygon([
    [-0.9147, 37.2602],
    [0.0688, 37.2602],
    [0.0688, 38.3019],
    [-0.9147, 38.3019]],{
        color: '#ff9505',
        fillColor: '#ffc971',
        fillOpacity: 0.1
    }).addTo(map);

