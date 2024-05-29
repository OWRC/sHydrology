

# leaflet map
output$map <- renderLeaflet({
  
  ormgp.bound <- read_sf("https://raw.githubusercontent.com/OWRC/geojson/main/ORMGP_region_2023.geojson") #("https://www.dropbox.com/s/lrdycz5eomw09hr/ORMGP_Area_20210205-Drawing-simplWGS.geojson?dl=1")
  
  leaflet(ormgp.bound) %>%
    addMouseCoordinates() %>%
    
    addScaleBar(position = "bottomright") %>%
    
    addFullscreenControl() %>%
    
    addTiles(attribution = '<a href="https://owrc.github.io/interpolants/#data-sources" target="_blank" rel="noopener noreferrer"><b>Source Data</b></a> © Oak Ridges Moraine Groundwater Program') %>%
    addTiles(group='OpenStreetMap') %>%

    # addTiles("https://tile.oakridgeswater.ca/solris/{z}/{x}/{y}", group = "SOLRIS", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Program")) %>%
    # addTiles("https://tile.oakridgeswater.ca/dem/{z}/{x}/{y}", group = "demtest", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Program", maxNativeZoom = 16)) %>% maxZoom = 18,
    addTiles("https://tile.oakridgeswater.ca/basemap/{z}/{x}/{y}", group = "ORMGP basemap", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Programx", maxNativeZoom = 17)) %>%
    addTiles("https://tile.oakridgeswater.ca/lidar24/{z}/{x}/{y}", group = "High definition DEM", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Programx", maxNativeZoom = 16)) %>%
    addTiles("https://tile.oakridgeswater.ca/wtdepth/{z}/{x}/{y}", group = "Depth to Watertable", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Program", maxNativeZoom = 16, opacity=.7)) %>%
    addTiles("https://tile.oakridgeswater.ca/landuse23/{z}/{x}/{y}", group = "Land Use", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Program", opacity=.7, maxNativeZoom = 16)) %>%
    addTiles("https://tile.oakridgeswater.ca/surfgeo23/{z}/{x}/{y}", group = "Surficial Geology", options = providerTileOptions(attribution=" © Oak Ridges Moraine Groundwater Program", opacity=.7, maxNativeZoom = 16)) %>%
    addTiles("https://tile.oakridgeswater.ca/topography/{z}/{x}/{y}", group = "Add hillshade & Topography", options = providerTileOptions(maxZoom=18, maxNativeZoom=16, attribution=" © Oak Ridges Moraine Groundwater Program")) %>%
    
    addLogo(
      img="ORMGP_logo_vsmall.png",
      src= "remote",
      position="bottomleft",
      offset.x = 10,
      offset.y = 10,
      width = 294
    ) %>%

    addMeasure(
      position = "topleft",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "hectares",
      secondaryAreaUnit = "acres",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    ) %>%
    
    # addEasyButton(easyButton(
    #   icon="fa-crosshairs", title="Locate Me",
    #   onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
        
    # addMarkers(lng = tblSta$LONG, lat = tblSta$LAT, icon = blueIcon) %>%
    setView(lng = mean(tblSta$LONG), lat = mean(tblSta$LAT), zoom = 8) %>%
    addPolygons(weight = 2,
                color = "black",
                fill=FALSE,
                dashArray = c(10, 5),
                opacity = .8,
                group = "ORMGP jurisdiction",
                options = pathOptions(clickable = FALSE)
    ) %>%
    addLayersControl (
      overlayGroups = "Add hillshade & Topography",
      baseGroups = c("ORMGP basemap", "OpenStreetMap", "High definition DEM", "Depth to Watertable", "Land Use", "Surficial Geology"), #"SOLRIS", "Topo", "Toner Lite"),
      options = layersControlOptions(position = "topleft")
    ) %>% hideGroup("Add hillshade & Topography") #%>%
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
  co <- NULL #if (input$chkClus) markerClusterOptions() else NULL
  
  m <- leafletProxy("map") %>%
    clearPopups() %>%
    clearMarkers() %>%
    clearMarkerClusters()

  if (input$chkSW) {
    if (!is.null(swlnk)) {  
      m %>% addMarkers(data = d,
                       layerId = ~INT_ID,
                       lng = ~LONG, lat = ~LAT,
                       label = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1),
                       icon = blueIcon,
                       popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1, #' (',LOC_ID,')',
                                        '<br><a href="',swlnk,LOC_ID,'" target="_blank">analyze streamflow data</a>',
                                        '<br><a href="https://wateroffice.ec.gc.ca/report/historical_e.html?stn=',LOC_NAME,'" target="_blank">ECCC source data</a>'),
                       clusterId = 1, clusterOptions = co)
    } else {
      m %>% addMarkers(data = d,
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       label = ~NAM1,
                       icon = blueIcon,
                       popup = ~paste0(NAM1,': ',NAM2),
                       clusterId = 1, clusterOptions = co)
    }
  }
  
  if (input$chkMet) {
    if (is.null(tblStaMet)) qMetLoc()
    if (!is.null(tblStaMet)){
      df <- filteredDataMet()
      if (nrow(df)>0) {
        m %>% addMarkers(data = df,
                         layerId = ~INT_ID,
                         lng = ~LONG, lat = ~LAT,
                         label = ~LOC_NAME,
                         icon = redIcon,
                         popup = ~paste0(LOC_NAME_ALT1,': ',LOC_NAME,
                                         '<br><a href="',metlnk,LOC_ID,'" target="_blank">analyze climate data</a>',
                                         '<br><a href="https://climate.weather.gc.ca/climate_data/daily_data_e.html?StationID=',aes_station_id,'" target="_blank">ECCC source data</a>'),
                         clusterId = 1, clusterOptions = co)        
      }
    }
  }
  
  if (input$chkGW) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      df <- filteredDataGW()
      if (nrow(df)>0) {
        m %>% addMarkers(data = df,
                         layerId = ~INT_ID,
                         lng = ~LONG, lat = ~LAT,
                         label = ~LOC_NAME,
                         icon = greenIcon,
                         popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1,'<br><a href="',gwlnk,INT_ID,'" target="_blank">analyze monitoring data</a>'),
                         clusterId = 1, clusterOptions = co)        
      }
    }
  }
  
  if (input$chkGWshal) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      df <- filteredDataGWshallow()
      if (nrow(df)>0) {
        m %>% addMarkers(data = df,
                         layerId = ~INT_ID,
                         lng = ~LONG, lat = ~LAT,
                         label = ~LOC_NAME,
                         icon = orangeIcon,
                         popup = ~paste0(LOC_NAME,': ',LOC_NAME_ALT1,'<br><a href="',gwlnk,INT_ID,'" target="_blank">analyze monitoring data</a>'),
                         clusterId = 1, clusterOptions = co)        
      }
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