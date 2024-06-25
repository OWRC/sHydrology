
capi <- "http://golang.oakridgeswater.ca:8080/carea/"
cidapi <- "http://golang.oakridgeswater.ca:8080/careacid/"

observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_click
  if (is.null(event)) return()
  
  lat <- round(event$lat,3)
  lng <- round(event$lng, 3)
  qstr <- paste0(capi,event$lat,"/",event$lng)
  geojson <- readLines(qstr) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON() %>%
            addPopups(event$lng,event$lat,paste0(lat, ', ', lng,
                                                 # '<br><a href="https://owrc.shinyapps.io/sHydrology/?lat=',lat,'&lng=',lng,'" target="_blank">view climatology..</a>'))
                                                 '<br><a href="https://owrc.shinyapps.io/sHyMetDS/?lat=',lat,'&lng=',lng,'" target="_blank">view climatology..</a>'))
                                                 ) %>%
    addGeoJSON(geojson)
})

drawCarea <- function(lat, lng) {
  url <- paste0(capi,lat,"/",lng)
  print(url)
  geojson <- readLines(url) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON()) %>%
    addGeoJSON(geojson)
}

drawCareaCid <- function(cid) {
  url <- paste0(cidapi,cid)
  print(url)
  geojson <- readLines(url) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON()) %>%
    addGeoJSON(geojson)
}