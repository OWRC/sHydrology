
capi <- "http://golang.oakridgeswater.ca:8080/carea/" # "http://localhost:8081/carea/" # 
cidapi <- "http://golang.oakridgeswater.ca:8080/careacid/" # "http://localhost:8081/careacid/" # 

drawCarea <- function(lat, lng) {
  url <- paste0(capi,lat,"/",lng)
  print(url)
  geojson <- readLines(url) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON()) %>%
    addGeoJSON(geojson)
  return(geojson)
}

drawCareaCid <- function(cid) {
  url <- paste0(cidapi,cid)
  print(url)
  geojson <- readLines(url) %>% paste(collapse = "\n")
  isolate(leafletProxy("map") %>%
            clearGeoJSON()) %>%
    addGeoJSON(geojson)
  return(geojson)
}