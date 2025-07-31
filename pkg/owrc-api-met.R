
getCDS <- function(url){
  out <- tryCatch(
    {
      jsonlite::fromJSON(url)
    },
    error=function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={}
  )
  out[out==-999] <- NA
  return(out)
}

getMeteo <- function(lat,lng) {
  # collect interpolated data
  url <- paste0('http://fews.oakridgeswater.ca:8080/dymetc/',lat,'/',lng)
  print(url)
  df <- getCDS(url)
  if ( length(df)==0) return(NULL)
  if ( length(df)==1 && df=="NA" ) return(NULL)
  df[df == -999] <- NA # do this before converting date
  df$Date = as.Date(df$Date)
  # df$Pa = df$Pa/1000 # to kPa
  return(df)  
}