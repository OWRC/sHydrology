

################################
##### map/object rendering #####
################################


source("pkg/icons.R", local = TRUE)$value
source("pkg/filteredData.R", local = TRUE)$value
source("pkg/dummy.R", local = TRUE)$value



# reactives
sta <- reactiveValues(loc=NULL, id=NULL, typ=NULL, name=NULL, name2=NULL, hyd=NULL, DTb=NULL, DTe=NULL)


# leaflet map
output$map <- renderLeaflet({
  m <- leaflet() %>%
    addTiles(group='OSM') %>% # OpenStreetMap by default
    addProviderTiles(providers$OpenTopoMap, group='Topo', options = providerTileOptions(attribution=" Map data: © OpenStreetMap contributors, SRTM | Map style: © OpenTopoMap (CC-BY-SA) | Oak Ridges Moraine Groundwater Program")) %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite", options = providerTileOptions(attribution=" Map tiles by Stamen Design, CC BY 3.0 — Map data © OpenStreetMap contributors | Oak Ridges Moraine Groundwater Program")) %>%
    # addMarkers(lng = tblSta$LONG, lat = tblSta$LAT, icon = blueIcon) %>%
    setView(lng = mean(tblSta$LONG), lat = mean(tblSta$LAT), zoom = 9) %>%
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


observe({
  if (!is.null(input$map_marker_click)){
    e <- input$map_marker_click
    sta$id <- e$id
    if (!is.null(sta$id)){
      if (input$chkSW) {
        if (sta$id %in% tblSta$INT_ID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblSta[tblSta$INT_ID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$typ <- 1
            sta$name <- as.character(starow$LOC_NAME)
            sta$name2 <- as.character(starow$LOC_NAME_ALT1)
            sta$hyd <- qTemporalSW(idbcsw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T) 
            drawCarea(starow$LAT,starow$LONG)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",swlnk,sta$loc,"', '_blank')")
          onclick("expnd", runjs(wlnk))
        }
      }
      if (input$chkMet) {
        if (sta$id %in% tblStaMet$INT_ID) {
           withProgress(message = 'Querying..', value = 0.1, {
             starow <- tblStaMet[tblStaMet$INT_ID==sta$id,]
             sta$loc <- starow$LOC_ID
             sta$typ <- 2
             sta$name <- as.character(starow$LOC_NAME_ALT1)
             sta$name2 <- as.character(starow$LOC_NAME)
             sta$hyd <- qTemporalMET(idbcmet,sta$id)
             incProgress(0.5, detail = 'Rendering plot..')
             sta$DTb <- min(sta$hyd$Date, na.rm=T)
             sta$DTe <- max(sta$hyd$Date, na.rm=T)
             setProgress(1)
           })
           shinyjs::enable("dnld")
           shinyjs::enable("expnd")
           wlnk <- paste0("window.open('",metlnk,sta$loc,"', '_blank')")
           onclick("expnd", runjs(wlnk))
        }
      }
      if (input$chkGW | input$chkGWshal) {
        if (sta$id %in% tblGW$INT_ID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblGW[tblGW$INT_ID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$typ <- 3
            sta$name <- as.character(starow$LOC_NAME)
            sta$name2 <- as.character(starow$LOC_NAME_ALT1)
            sta$hyd <- qTemporalGW(idbcgw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",gwlnk,sta$id,"', '_blank')")
          onclick("expnd", runjs(wlnk))          
        }
      }
    }
  }
})

# observeEvent(input$map_draw_new_feature, { # see: https://redoakstrategic.com/geoshaper/
#   found_in_bounds <- findLocations(shape = input$map_draw_new_feature
#                                    , location_coordinates = coordinates
#                                    , location_id_colname = "IID")
#   print(found_in_bounds)
# })

# Dygraph hydrograph preview in absolutePanel
output$hydgrph <- renderDygraph({
  if (!is.null(sta$hyd)){
    switch(sta$typ,
           { # 1=surface water
             qxts <- xts(sta$hyd$Flow, order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$Flow))-2)*8) # dynamic plot fitting
             colnames(qxts) <- 'Discharge'
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
               dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
               dyRangeSelector(strokeColor = '')                 
           },
           { # 2=climate
             qxts <- xts(cbind(sta$hyd$precipitation_amount,sta$hyd$mean_air_temperature), order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$precipitation_amount))-2)*8) # dynamic plot fitting
             colnames(qxts) <- c('precipitation_amount','mean_air_temperature')
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
               dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
               dyRangeSelector(strokeColor = '')  
           },
           { # 3=gw
             h1 <- sta$hyd[year(sta$hyd$Date)>1900,]
             qxts <- xts(h1, order.by = h1$Date)
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE) %>%
               dyAxis(name='y', labelWidth=0) %>%
               dyRangeSelector(strokeColor = '')  
           })
  }
})

output$dnld <- downloadHandler(
  filename <- function() { paste0(sta$name, '.csv') },
  content <- function(file) {
    switch(sta$typ,
           { # 1=surface water
             if(!is.null(sta$hyd)) write.csv(sta$hyd[!is.na(sta$hyd$Flow),], file, row.names = FALSE)               
           },
           { # 2=climate
             if(!is.null(sta$hyd)) write.csv(sta$hyd[colSums(!is.na(sta$hyd)) > 0], file, row.names = FALSE)
           },
           { # 3=gw monitoring
             if(!is.null(sta$hyd)) write.csv(sta$hyd, file, row.names = FALSE)
           })
  } 
)
