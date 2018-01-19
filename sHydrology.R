##########################################################
################### sHydrology_map ####################### 
## A Shiny-Leaflet interface to the WSC HYDAT database. ##
##########################################################
# By M. Marchildon
# v.1.0
# Jan 19, 2018
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


dbc <- dbcnxn('<...path of Hydat.sqlite3 file goes here...>')
tblSta <- qStaLoc(dbc)
# rlnk <- ''

shinyApp(
  ui <- bootstrapPage(
    useShinyjs(),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeCSS("styles.css")),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(id = "panl", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 10, left = "auto", right = 10, bottom = "auto",
                  width = 330, height = "auto",
                
                  h2("Hydrograph explorer"),
                  sliderInput("YRrng", "Select date range", min(tblSta$StartYear), max(tblSta$EndYear),
                              value = c(min(tblSta$StartYear),max(tblSta$EndYear)), sep=""),
                  
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
      leaflet(tblSta) %>%
        addTiles() %>%
        addMarkers()
    })
    
    observe({
      leafletProxy("map", data = filteredData()) %>%
        clearPopups() %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        addMarkers(layerId = ~sID,
                   popup = ~paste0(sName,': ',sName2),
                   clusterOptions = markerClusterOptions())
    })
    
    filteredData <- reactive({
      tblSta[tblSta$EndYear >= input$YRrng[1] & tblSta$StartYear <= input$YRrng[2],]
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
          withProgress(message = 'Rendering plot..', value = 0.1, {
            starow <- tblSta[tblSta$sID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$name <- as.character(starow$sName)
            sta$name2 <- as.character(starow$sName2)
            sta$hyd <- qTemporal(dbc,sta$id)
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T)          
            setProgress(1)
          })
          shinyjs::enable("dnld")
          # shinyjs::enable("expnd")
          # wlnk <- paste0("window.open('",rlnk,"?sID=",sta$loc,"', '_blank')")
          # onclick("expnd", runjs(wlnk))
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
