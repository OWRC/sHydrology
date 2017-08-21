

library(leaflet)
library(shiny)
library(shinyBS)
library(ggplot2)

source("functions/HYDAT_query.R")
source("functions/hydrograph_separation.R")



###########################################################################################
## leaflet addTiles fix from kgavhane702 (https://github.com/rstudio/leaflet/issues/192) ##
###########################################################################################
addTiles = function (map, urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                     attribution = NULL, layerId = NULL, group = NULL, options = tileOptions()){
  options$attribution = attribution
  if (missing(urlTemplate) && is.null(options$attribution))
    options$attribution = paste("© OpenStreetMap"," | datasource: WSC HYDAT")
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate, layerId, group, options)
}
###########################################################################################




dbc <- dbConn("db/Hydat.sqlite3")
tblSta <- StaLocations(dbc)
# View(tblSta)




###########################################################################################
## Shiny
###########################################################################################
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h4("WSC HYDAT explorer"),
                sliderInput("YRrng", "Year", min(tblSta$StartYear), max(tblSta$EndYear),
                            value = c(min(tblSta$StartYear),max(tblSta$EndYear)), sep="")
                # textInput("text", label = h3("Text input"), value = "Enter text...")
                ),
  absolutePanel(bottom = 5, left = 5, right = 5,
                bsCollapse(id = "cpnl",
                           bsCollapsePanel("View hydrograph", value = "pnlhyd", style = "primary",
                                           tabsetPanel(
                                             tabPanel("Hydrograph",
                                                      fluidRow(
                                                        column(3, dateRangeInput("DTrng", label = "Date range")),
                                                        column(6, br(), sliderInput("DTsld", NULL, width='200%', min = as.Date('1900-01-01'), max = as.Date('2000-1-1'), value = c(as.Date('1900-1-1'),as.Date('2000-1-1')), ticks = F, timeFormat = "%b-%Y")),
                                                        column(3, br(), checkboxInput("chkBF", "Separate baseflow", value = FALSE))
                                                      ),
                                                      plotOutput("hydgrphM", height = '100px', 
                                                                 dblclick = "hydgrphM_dblclick",
                                                                 brush=brushOpts(id = "hydgrphM_brush",direction="x",resetOnNew=F)),
                                                      plotOutput("hydgrph", height = '300px',
                                                                 dblclick = "hydgrph_dblclick", 
                                                                 brush=brushOpts(id = "hydgrph_brush",direction="x",resetOnNew=T))
                                             ),
                                             tabPanel("Data",
                                                      fluidRow(
                                                        column(2, br(), downloadButton("btnCsv", "Download csv..")),
                                                        column(10, dataTableOutput('tabhyd'))
                                                      )
                                             )
                                           )
                           )
                )
  )  
)
###########################################################################################
server <- function(input, output, session) {
  sta <- reactiveValues(station = NULL, carea=NULL, maxQ=NULL, hyd=NULL, DTb=NULL, DTe=NULL, BFbuilt=FALSE)
  
  ################################
  ## map/object rendering
  # Reactive expression for the data subsetted to what the user selected

  output$map <- renderLeaflet({
    leaflet(tblSta) %>%
      addTiles() %>%
      addMarkers()
  })
  
  output$tabhyd <- renderDataTable({
    if (!is.null(sta$hyd)){
      sta$hyd[!is.na(sta$hyd$Flow),] #return(sta$hyd)
    }
  }, options = list(scrollY='300px', scrollX=TRUE,
                    lengthMenu = c(30, 100, 365, 3652), 
                    pageLength = 100,
                    searching=FALSE)
  )
  
  output$hydgrphM <- renderPlot({
    if (!is.null(sta$hyd)){
      ggplot(sta$hyd, aes(x = Date, y = Flow)) +
        theme_bw() +
        geom_line() +
        coord_cartesian(xlim = c(sta$DTb,sta$DTe)) +
        labs(y = "Q (m³/s)", x=NULL) +
        scale_x_date()
    }
  })
  
  output$hydgrph <- renderPlot({
    if (!is.null(sta$hyd)){
      if (!input$chkBF | !sta$BFbuilt){
        ggplot(sta$hyd, aes(x = Date, y = Flow)) +
          theme_bw() +
          geom_line(color="blue", size=1) +
          coord_cartesian(xlim = input$DTrng, ylim = c(0,sta$maxQ)) +
          labs(y = "Discharge (m³/s)", x=NULL) +
          scale_x_date()
      } else {
        ggplot(sta$hyd, aes(x = Date, y = Flow)) +
          theme_bw() +
          geom_line(color="blue", size=1) +
          geom_ribbon(aes(ymin=BF.min,ymax=BF.max), fill="green", alpha="0.3") +
          geom_line(aes(y=BF.med), color="black", linetype="dotted", size=2) +
          coord_cartesian(xlim = input$DTrng, ylim = c(0,sta$maxQ)) +
          labs(y = "Discharge (m³/s)", x=NULL) +
          scale_x_date()
      }
    }
  })  
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(layerId = ~STATION_NUMBER,
                 popup = ~paste0(STATION_NUMBER,': ',STATION_NAME),
                 clusterOptions = markerClusterOptions())
  })
  
  filteredData <- reactive({
    tblSta[tblSta$EndYear >= input$YRrng[1] & tblSta$StartYear <= input$YRrng[2],]
  })

  
  ################################
  ## events
  observe({
    if (!is.null(input$map_marker_click)){
      e <- input$map_marker_click
      sta$station <- e$id
      resetPnl()
      # updateTextInput(session, "text", value = sta$station)
    }
  })
  
  resetPnl <- function(){
    sta$BFbuilt <- FALSE
    sta$carea = NULL
    sta$maxQ = NULL
    sta$hyd = NULL
    session$resetBrush('hydgrphM_brush')
    updateCheckboxInput(session,'chkBF',value=FALSE)
    updateCollapse(session,'cpnl',close='pnlhyd')
  }
  
  observeEvent(input$DTrng,{
    isolate({
      updateSliderInput(session, "DTsld", value = c(input$DTrng[1],input$DTrng[2]))
    })
  })
  
  observeEvent(input$DTsld,{
    isolate({
      updateDateRangeInput(session, "DTrng", start = input$DTsld[1], end = input$DTsld[2])
      hydgrphM_brush <- input$DTsld      
    })
  })
  
  ## hydgrph events
  observeEvent(input$hydgrph_dblclick, {
    updateDateRangeInput(session, "DTrng", start = sta$DTb, end = sta$DTe)
    session$resetBrush('hydgrphM_brush')
  })
  
  observeEvent(input$hydgrph_brush,{
    brush <- input$hydgrph_brush
    if (!is.null(brush)) {
      updateDateRangeInput(session, "DTrng", start = as.Date(brush$xmin), end = as.Date(brush$xmax))
      session$resetBrush('hydgrphM_brush')
    }
  })
  
  ## hydgrphM events
  observeEvent(input$hydgrphM_dblclick, {
    updateDateRangeInput(session, "DTrng", start = sta$DTb, end = sta$DTe)
    session$resetBrush('hydgrphM_brush')
  })  
  
  observeEvent(input$hydgrphM_brush,{
    brush <- input$hydgrphM_brush
    if (!is.null(brush)) {
      # updateTextInput(session, "text", value = nrngM)
      updateDateRangeInput(session, "DTrng", start = as.Date(brush$xmin), end = as.Date(brush$xmax))
    }
  })
  
  observeEvent(input$chkBF,{
    if(input$chkBF){separateHydrograph()}
  })
  
  separateHydrograph <- function(){
    # progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Separating hydrograph..", detail = 'initializing..', value = 0.1)
    on.exit(progress$close())
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    if (!is.null(sta$hyd) & !sta$BFbuilt){
      sta$hyd <- baseflow_range(sta$hyd,sta$carea,updateProgress)
      sta$BFbuilt <- TRUE
      progress$set(value = 1)
    }
  }
  
  observeEvent(input$cpnl,({
    updatePnl1()
  }))
  
  updatePnl1 <- function(){
    if (!is.null(sta$station)){
      withProgress(message = 'Creating plot', value = 0.1, {
        sta$hyd <- qTemporal(dbc,sta$station)
        sta$carea <- qStaCarea(dbc,sta$station)
        sta$BFbuilt <- FALSE
        sta$maxQ <- as.numeric(quantile(sta$hyd$Flow, .99, na.rm=T))
        sta$DTb <- min(sta$hyd$Date, na.rm=T)
        sta$DTe <- max(sta$hyd$Date, na.rm=T)  
        # updateTextInput(session, "text", value = paste0(sta$station, ' ',sta$maxQ))
        isolate({
          updateSliderInput(session, "DTsld", value = c(sta$DTb,sta$DTe), min = sta$DTb, max = sta$DTe)
        })
        isolate({
          updateDateRangeInput(session, "DTrng", label = sta$station, start = sta$DTb, end = sta$DTe, min = sta$DTb, max = sta$DTe)
        })
        setProgress(1)
      })

    }    
  }
  
  output$btnCsv <- downloadHandler(
    filename = function() { paste0(sta$station, '.csv') },
    content = function(file) {
      if (!is.null(sta$hyd)){write.csv(sta$hyd[!is.na(sta$hyd$Flow),], file, row.names = FALSE)}
    }
  )
}

shinyApp(ui, server)
###########################################################################################









# # general plot
# ggplot(hyd, aes(x = Date, y = Flow)) +
#   theme_bw() +
#   geom_line() +
#   labs(y = "Discharge (m³/s)",
#        title = staID)
# 
# 
# y <- data.frame(
#   date=format(index(hyd), '%Y-%m-%d'),
#   flow=as.numeric(hyd),
#   flag=Flag
# )
# write.csv(y, file = 'hydat_out1.csv', row.names=FALSE)



# observe({
#   if (!is.null(input$map_marker_mouseout)){
#     e <- input$map_marker_mouseout
#     removePopup(leafletProxy("map"),e$id)
#     # leafletProxy("map") %>% clearPopups()
#   }
# })

# observe({
#   leafletProxy("map") %>% clearPopups()
#   if (!is.null(input$map_marker_mouseover)){
#     e <- input$map_marker_mouseover
#     offset_lat <- 0.3 * cos(e$lat*pi/180)/2^(input$map_zoom+8)*40075.017
#     updateTextInput(session, "text", value = offset_lat)
#     isolate({
#       showStationPopup(e$id, offset_lat + e$lat, e$lng)
#     })
#   }
# })
# 
# showStationPopup <- function(stationNum, lat, lng){
#   # sel <- tblSta[tblSta$STATION_NUMBER == stationNum]
#   content <- as.character(tagList(
#     tags$h5(stationNum)#,
#     # sel$STATION_NAME
#   ))
#   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = stationNum, 
#                                     options = popupOptions(autoPan = FALSE))
# }