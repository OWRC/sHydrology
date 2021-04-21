
observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_click
  if (is.null(event)) return()

  qstr <- paste0(capi,event$lat,"/",event$lng)
  geojson <- readLines(qstr) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON() %>%
            addPopups(event$lng,event$lat,paste0(round(event$lat,3), ', ', round(event$lng, 3)))) %>%
    addGeoJSON(geojson)
})

drawCarea <- function(lat, lng) {
  geojson <- readLines(paste0(capi,lat,"/",lng)) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON()) %>%
    addGeoJSON(geojson)
}