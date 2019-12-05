#################################################################################################
##### leaflet addTiles fix from kgavhane702 (https://github.com/rstudio/leaflet/issues/192) #####
#################################################################################################
addTiles = function (map, urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                     attribution = NULL, layerId = NULL, group = NULL, options = tileOptions()){
  options$attribution = attribution
  if (missing(urlTemplate) && is.null(options$attribution)){
    options$attribution = "&copy; OpenStreetMap | Oak Ridges Moraine Groundwater Program"
  }
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate, layerId, group, options)
}
#################################################################################################