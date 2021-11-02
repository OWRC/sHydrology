



################################
## map/object rendering


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
    addMarkers(lng = tblSta$LNG, lat = tblSta$LAT, icon = blueIcon) %>%
    addLayersControl (
      baseGroups = c("Topo", "OSM", "Toner Lite"),
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
  clus <- NULL
  if (input$chkCluster) clus <- markerClusterOptions()
  
  m <- leafletProxy("map") %>%
    clearPopups() %>%
    clearMarkers() %>%
    clearMarkerClusters()
  
  if (input$chkSW) {
    if (!is.null(swlnk)) {  
      m %>% addMarkers(data = d,
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       icon = blueIcon,
                       popup = ~paste0(NAM1,': ',NAM2),
                       clusterId = 1, clusterOptions = clus)
    } else {
      m %>% addMarkers(data = d,
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       icon = blueIcon,
                       popup = ~paste0(NAM1,': ',NAM2,'<br><a href="',swlnk,LID,'" target="_blank">analyze streamflow data</a>'),
                       clusterId = 1, clusterOptions = clus)
    }
  }

  if (input$chkMet) {
    if (is.null(tblStaMet)) qMetLoc()
    if (!is.null(tblStaMet)){
      m %>% addMarkers(data = filteredDataMet(),
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       icon = redIcon,
                       popup = ~paste0(NAM1,': ',NAM2,'<br><a href="',metlnk,LID,'" target="_blank">analyze climate data</a>'),
                       clusterId = 1, clusterOptions = clus)
    }
  }

  if (input$chkGW) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      m %>% addMarkers(data = filteredDataGW(),
                       layerId = ~IID,
                       lng = ~LNG, lat = ~LAT,
                       icon = greenIcon,
                       popup = ~paste0(NAM1,': ',NAM2,'<br><a href="',gwlnk,IID,'" target="_blank">analyze monitoring data</a>'),
                       clusterId = 1, clusterOptions = clus)
    }
  }
  
  if (input$chkGWshal) {
    if (is.null(tblGW)) qGWLoc()
    if (!is.null(tblGW)){
      m %>% addMarkers(data = filteredDataGWshallow(),
                   layerId = ~IID,
                   lng = ~LNG, lat = ~LAT,
                   icon = orangeIcon,
                   popup = ~paste0(NAM1,': ',NAM2,'<br><a href="',gwlnk,IID,'" target="_blank">analyze monitoring data</a>'),
                   clusterId = 1, clusterOptions = clus)
    }    
  }
  
  
  if (input$chkCluster) {
    m %>% clearMarkers()
  } else {
    m
  }
})


observe({
  if (!is.null(input$map_marker_click)){
    e <- input$map_marker_click
    sta$id <- e$id
    if (!is.null(sta$id)){
      if (input$chkSW) {
        if (sta$id %in% tblSta$IID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblSta[tblSta$IID==sta$id,]
            sta$loc <- starow$LID
            sta$typ <- 1
            sta$name <- as.character(starow$NAM1)
            sta$name2 <- as.character(starow$NAM2)
            sta$hyd <- qTemporalSW(idbcsw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T) 
            drawCarea(starow$LAT,starow$LNG)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",swlnk,sta$loc,"', '_blank')")
          onclick("expnd", runjs(wlnk))
        }
      }
      if (input$chkMet) {
        if (sta$id %in% tblStaMet$IID) {
           withProgress(message = 'Querying..', value = 0.1, {
             starow <- tblStaMet[tblStaMet$IID==sta$id,]
             sta$loc <- starow$LID
             sta$typ <- 2
             sta$name <- as.character(starow$NAM1)
             sta$name2 <- as.character(starow$NAM2)
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
        if (sta$id %in% tblGW$IID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblGW[tblGW$IID==sta$id,]
            sta$loc <- starow$LID
            sta$typ <- 3
            sta$name <- as.character(starow$NAM1)
            sta$name2 <- as.character(starow$NAM2)
            sta$hyd <- qTemporalGW(idbcgw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",gwlnk,sta$loc,"', '_blank')")
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
             qxts <- xts(cbind(sta$hyd$Pre,sta$hyd$Tem), order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$Pre))-2)*8) # dynamic plot fitting
             colnames(qxts) <- c('Precip','Temp')
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
             if(!is.null(sta$hyd)) write.csv(sta$hyd, file, row.names = FALSE)
           },
           { # 3=gw monitoring
             if(!is.null(sta$hyd)) write.csv(sta$hyd, file, row.names = FALSE)
           })
  } 
)
