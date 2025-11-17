
capi <- "http://golang.oakridgeswater.ca:8080/carea/" # "http://localhost:8081/carea/" # 
cidapi <- "http://golang.oakridgeswater.ca:8080/careacid/" # "http://localhost:8081/careacid/" # 

get_bbox <- function(geojson) {
  geojson_parsed <- fromJSON(geojson, simplifyVector = FALSE)
  coords <- geojson_parsed$features[[1]]$geometry$coordinates[[1]] # first ring
  df <- as.data.frame(do.call(rbind, lapply(coords, function(x) unlist(x))),
                      stringsAsFactors = FALSE)
  colnames(df) <- c("lon", "lat")
  list(lng1 = min(df$lon), lat1 = min(df$lat),
       lng2 = max(df$lon), lat2 = max(df$lat))
}

drawCarea <- function(lat, lng) {
  url <- paste0(capi,lat,"/",lng)
  print(url)
  geojson <- readLines(url, warn = FALSE) %>% paste(collapse = "\n")
  bbox <- get_bbox(geojson)
  isolate(leafletProxy("map") %>% clearGeoJSON()) %>%
    addGeoJSON(geojson) |>
    fitBounds(bbox$lng1, bbox$lat1, bbox$lng2, bbox$lat2)
  return(geojson)
}

drawCareaCid <- function(cid) {
  url <- paste0(cidapi,cid)
  print(url)
  geojson <- readLines(url, warn = FALSE) %>% paste(collapse = "\n")
  bbox <- get_bbox(geojson)
  isolate(leafletProxy("map") %>% clearGeoJSON()) %>%
    addGeoJSON(geojson) |>
    fitBounds(bbox$lng1, bbox$lat1, bbox$lng2, bbox$lat2)
  return(geojson)
}