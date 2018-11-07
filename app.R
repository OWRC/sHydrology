##########################################################
################### sHydrology_map ####################### 
#### A Shiny-Leaflet interface to the YPDT database.  ####
##########################################################
# Leaflet map
#
# By M. Marchildon
# v.1.2.1
# Nov, 2018
##########################################################

library(shiny)
library(shinyjs)
library(jsonlite)
library(leaflet)
library(zoo)
library(dygraphs)
library(xts)

source("functions/shiny_leaflet_functions.R")
source("functions/HYDAT_query.R")


#### Links to sHydrology analysis (ORMGP API)
swlnk <- NULL
metlnk <- NULL


shinyApp(
  ui <- bootstrapPage(
    useShinyjs(),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeCSS("functions/styles.css")),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(id = "panl", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 10, left = "auto", right = 10, bottom = "auto",
                  width = 330, height = "auto",
                  
                  h2("Hydrograph explorer"),
                  sliderInput("YRrng", "Select date envelope", min(tblSta$YRb), max(tblSta$YRe),
                              value = c(max(tblSta$YRe)-30,max(tblSta$YRe)), sep=""),
                  
                  selectInput("POR", "minimum period of length/count of data", c("no limit" = 0, "5yr" = 5, "10yr" = 10, "30yr" = 30)),
                  checkboxInput("chkMet", "show climate stations", FALSE),
                  
                  h4("Hydrograph preview:"),
                  dygraphOutput("hydgrph", height = 200), br(),
                  div(style="display:inline-block",actionButton("expnd", "Analyze")),
                  div(style="display:inline-block",downloadButton('dnld', 'Download CSV'))
    )
  ),
  
  
  server <- function(input, output, session) {
    shinyjs::disable("dnld")
    shinyjs::disable("expnd")
    
    
    ################################
    ## map/object rendering
    
    # leaflet map
    output$map <- renderLeaflet({
      m <- leaflet() %>%
        addTiles() %>%
        addMarkers(lng = tblSta$LNG, lat = tblSta$LAT)
    })
    
    observe({
      m <- leafletProxy("map") %>%
        clearPopups() %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        addMarkers(data = filteredData(),
                   layerId = ~IID, clusterId = 1,
                   lng = ~LNG, lat = ~LAT,
                   popup = ~paste0(NAM1,': ',NAM2),
                   clusterOptions = markerClusterOptions())
      if (input$chkMet) {
        m %>% addCircleMarkers(data = filteredDataMet(),
                         layerId = ~IID, clusterId = 2,
                         lng = ~LNG, lat = ~LAT,
                         popup = ~paste0(NAM1,': ',NAM2,'<br>', shiny::a("analyse climate data", href=paste0(metlnk, LID), target="_blank")),
                         clusterOptions = markerClusterOptions(),
                         color = 'red', radius = 6)
      }
    })
    
    filteredData <- reactive({
      p <- as.numeric(input$POR)*365.25
      tblSta[tblSta$YRe >= input$YRrng[1] & tblSta$YRb <= input$YRrng[2] & tblSta$CNT > p,]
    })
    
    filteredDataMet <- reactive({
      p <- as.numeric(input$POR)*365.25
      tblStaMet[tblStaMet$YRe >= input$YRrng[1] & tblStaMet$YRb <= input$YRrng[2] & tblStaMet$pcnt > p,]   
    })

    # hydrograph preview
    output$hydgrph <- renderDygraph({
      if (!is.null(sta$hyd)){
        qxts <- xts(sta$hyd$Flow, order.by = sta$hyd$Date)
        lw <- max(20,25 + (log10(max(sta$hyd$Flow))-2)*8) # dynamic plot fitting
        colnames(qxts) <- 'Discharge'
        dygraph(qxts) %>%
          # dyLegend(show = 'always') %>%
          dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
          dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
          dyRangeSelector(strokeColor = '')
      }
    })
    
    # reactives
    sta <- reactiveValues(loc=NULL, id=NULL, name=NULL, name2=NULL, hyd=NULL, DTb=NULL, DTe=NULL)
    
    observe({
      if (!is.null(input$map_marker_click)){
        e <- input$map_marker_click
        sta$id <- e$id
        if (!is.null(sta$id)){
          if (e$clusterId == 1){
            withProgress(message = 'Rendering plot..', value = 0.1, {
              starow <- tblSta[tblSta$IID==sta$id,]
              sta$loc <- starow$LID
              sta$name <- as.character(starow$NAM1)
              sta$name2 <- as.character(starow$NAM2)
              sta$hyd <- qTemporalSW(idbc,sta$id)
              sta$DTb <- min(sta$hyd$Date, na.rm=T)
              sta$DTe <- max(sta$hyd$Date, na.rm=T)          
              setProgress(1)
            })
            shinyjs::enable("dnld")
            shinyjs::enable("expnd")
            wlnk <- paste0("window.open('",swlnk,sta$loc,"', '_blank')")
            onclick("expnd", runjs(wlnk))            
          } else {
            print(e)
          }
        }
      }
    })
    
    output$dnld <- downloadHandler(
      filename <- function() { paste0(sta$name, '.csv') },
      content <- function(file) {
        if(!is.null(sta$hyd)) write.csv(sta$hyd[!is.na(sta$hyd$Flow),], file, row.names = FALSE)
      } 
    )
    
    session$onSessionEnded(stopApp)
  }
)
