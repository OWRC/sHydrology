##########################################################
################### sHydrologyMap ########################
#### A Shiny-Leaflet interface to the HYDAT database. ####
##########################################################
# Leaflet map
#
# By M. Marchildon
# v.1.7
# Dec, 2022
##########################################################


source("pkg/pkg.R", local=TRUE)
# source("pkg/shiny_leaflet_functions.R", local=TRUE)
source("pkg/sources.R", local=TRUE)
source("pkg/mobile.R", local=TRUE)


shinyApp(
  ui <- bootstrapPage(
    useShinyjs(),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    tags$head(includeCSS("pkg/styles.css")),
    
    div(
      id = "app-content",
      list(tags$head(HTML('<link rel="icon", href="favicon.png",type="image/png" />'))),
      div(style="padding: 1px 0px; height: 0px", titlePanel(title="", windowTitle="sHydrology map"))
    ),
    
    leafletOutput("map", width = "100%", height = "100%"),
    # windowTitle="sHydrology",
    
    # modified from superZip (http://shiny.rstudio.com/gallery/superzip-example.html)
    absolutePanel(id = "panl", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 10, left = "auto", right = 10, bottom = "auto",
                  width = 430, height = "auto",
                  
                  h2("Hydrograph explorer"),
                  sliderInput("YRrng", "Select date envelope", min(tblSta$YRb), max(tblSta$YRe),
                              value = c(max(tblSta$YRe)-30,max(tblSta$YRe)), sep="", width = "auto"),
                  
                  selectInput("POR", "minimum period of length/count of data", c("no limit" = 0, "5yr" = 5, "10yr" = 10, "30yr" = 30, "50yr" = 50, "75yr" = 75, "100yr" = 100)),
                  
                  checkboxInput("chkSW", "show streamflow stations", TRUE),
                  checkboxInput("chkMet", "show climate stations", FALSE),
                  checkboxInput("chkGW", "deep (>20m) groundwater monitoring", FALSE),
                  checkboxInput("chkGWshal", "shallow groundwater monitoring", FALSE),
                  
                  # h4("Hydrograph preview:"),
                  div(textOutput("legendDivID")),
                  dygraphOutput("hydgrph", height = 240), br(),
                  div(style="display:inline-block",actionButton("expnd", "Open in New Tab")),
                  div(style="display:inline-block",downloadButton('dnld', 'Download CSV'))                  
    ),
    mobileDetect('isMobile') ## from: https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
  ),
  
  
  server <- function(input, output, session) {
    shinyjs::disable("dnld")
    shinyjs::disable("expnd")
    
    ### load external code:
    source("pkg/server.R", local = TRUE)$value
    source("pkg/mobile_srv.R", local = TRUE)$value
    
    session$onSessionEnded(stopApp)
  }
)
