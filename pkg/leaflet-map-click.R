

observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_click
  if (is.null(event)) return()
  
  lat <- round(event$lat,3)
  lng <- round(event$lng, 3)
  url <- paste0(capi,event$lat,"/",event$lng)
  print(url)
  geojson <- readLines(url) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON() %>%
            addPopups(event$lng,event$lat,paste0(lat, ', ', lng,
                                                 # '<br><a href="https://owrc.shinyapps.io/sHydrology/?lat=',lat,'&lng=',lng,'" target="_blank">view climatology..</a>'))
                                                 '<br><a href="https://owrc.shinyapps.io/sHyMetDS/?lat=',lat,'&lng=',lng,'" target="_blank">view climatology..</a>'))
  ) %>%
    addGeoJSON(geojson)
})