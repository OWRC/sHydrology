

# leaflet map
output$map <- renderLeaflet({
  ormgp.bound <- rgdal::readOGR("https://www.dropbox.com/s/lrdycz5eomw09hr/ORMGP_Area_20210205-Drawing-simplWGS.geojson?dl=1") 
  leaflet(ormgp.bound) %>%
    addTiles(group='OSM') %>% # OpenStreetMap by default
    addProviderTiles(providers$OpenTopoMap, group='Topo', options = providerTileOptions(attribution=" Map data: © OpenStreetMap contributors, SRTM | Map style: © OpenTopoMap (CC-BY-SA) | Oak Ridges Moraine Groundwater Program")) %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(attribution=" Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap contributors | Oak Ridges Moraine Groundwater Program")) %>%
    # addMarkers(lng = tblSta$LONG, lat = tblSta$LAT, icon = blueIcon) %>%
    setView(lng = mean(tblSta$LONG), lat = mean(tblSta$LAT), zoom = 9) %>%
    addPolygons(weight = 2, 
                color = "black", 
                fill=FALSE, 
                dashArray = c(10, 5), 
                opacity = .8, 
                group = "ORMGP jurisdiction", 
                options = pathOptions(clickable = FALSE)
    ) %>%
    addLayersControl (
      baseGroups = c("OSM", "Topo", "Toner Lite"),
      options = layersControlOptions(position = "bottomleft")
    ) #%>%
  # addDrawToolbar(
  #   targetGroup='Selected',
  #   polylineOptions=FALSE,
  #   markerOptions = FALSE,
  #   # polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
  #   # rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
  #   # circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
  #   circleMarkerOptions = FALSE,
  #   editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())
  # )
})



observe({
  d <- filteredDataSW()
  
  m <- leafletProxy("map") %>%
    clearPopups() %>%
    clearMarkers() %>%
    clearMarkerClusters()
  
  if (input$chkSW) {
    if (!is.null(swlnk)) {  
      m %>% addMarkers(data = d,
                       layerId = ~INT_ID,
                       lng = ~LONG, lat = ~LAT,
                       label = ~LOC_NAME,
                       icon = blueIcon,
                       popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1,'<br><a href="',swlnk,LOC_ID,'" target="_blank">analyze streamflow data</a>'),
                       clusterId = 1, clusterOptions = NULL)
    } else {
      m %>% addMarkers(data = d,
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       label = ~NAM1,
                       icon = blueIcon,
                       popup = ~paste0(NAM1,': ',NAM2),
                       clusterId = 1, clusterOptions = NULL)
    }
  }
  
  if (input$chkMet) {
    if (is.null(tblStaMet)) qMetLoc()
    if (!is.null(tblStaMet)){
      m %>% addMarkers(data = filteredDataMet(),
                       layerId = ~INT_ID,
                       lng = ~LONG, lat = ~LAT,
                       label = ~LOC_NAME,
                       icon = redIcon,
                       popup = ~paste0(LOC_NAME_ALT1,': ',LOC_NAME,'<br><a href="',metlnk,LOC_ID,'" target="_blank">analyze climate data</a>'),
                       clusterId = 1, clusterOptions = NULL)
    }
  }
  
  if (input$chkGW) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      m %>% addMarkers(data = filteredDataGW(),
                       layerId = ~INT_ID,
                       lng = ~LONG, lat = ~LAT,
                       label = ~LOC_NAME,
                       icon = greenIcon,
                       popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1,'<br><a href="',gwlnk,INT_ID,'" target="_blank">analyze monitoring data</a>'),
                       clusterId = 1, clusterOptions = NULL)
    }
  }
  
  if (input$chkGWshal) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      m %>% addMarkers(data = filteredDataGWshallow(),
                       layerId = ~INT_ID,
                       lng = ~LONG, lat = ~LAT,
                       label = ~LOC_NAME,
                       icon = orangeIcon,
                       popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1,'<br><a href="',gwlnk,INT_ID,'" target="_blank">analyze monitoring data</a>'),
                       clusterId = 1, clusterOptions = NULL)
    }    
  }
  
  return(m)
})




# observeEvent(input$map_draw_new_feature, { # see: https://redoakstrategic.com/geoshaper/
#   found_in_bounds <- findLocations(shape = input$map_draw_new_feature
#                                    , location_coordinates = coordinates
#                                    , location_id_colname = "IID")
#   print(found_in_bounds)
# })