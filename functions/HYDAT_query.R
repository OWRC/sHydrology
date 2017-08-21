##########################################################
############## HYDAT/SQLite querying ##################### 
##########################################################
# By M. Marchildon
#
# Aug 21, 2017
##########################################################

library(RSQLite)
library(zoo)


###########################################################################################
## connect to the HYDAT sqlite3 file
###########################################################################################
dbConn <- function(dbName){
  dbc <- dbConnect(RSQLite::SQLite(), dbname=dbName)
  # get a list of all tables
  # dbListTables(dbc)  
  return(dbc)
}


###########################################################################################
## collect locations
###########################################################################################
StaLocations <- function(dbc, prov = "ON"){

  # get the STATIONS table as a data.frame
  tblSta <- dbGetQuery(dbc,'select * from STATIONS')
  tblSta <- tblSta[tblSta$PROV_TERR_STATE_LOC == "ON",]
  tblSta <- tblSta[!is.na(tblSta$DRAINAGE_AREA_GROSS),]
  tblSta <- tblSta[!is.na(tblSta$LATITUDE),]
  
  # query date ranges
  YRb <- vector('numeric',length=length(tblSta$STATION_NUMBER))
  YRe <- vector('numeric',length=length(tblSta$STATION_NUMBER))
  cnt <- 0
  for (s in tblSta$STATION_NUMBER){
    q <- dbGetQuery(dbc, paste0('select * from DLY_FLOWS where STATION_NUMBER = "',s,'"'))
    cnt <- cnt + 1
    YRb[cnt] <- min(q$YEAR)
    YRe[cnt] <- max(q$YEAR)
  }
  tblSta$StartYear <- YRb
  tblSta$EndYear <- YRe
  tblSta <- tblSta[!is.infinite(tblSta$StartYear),]
  tblSta <- tblSta[!is.na(tblSta$StartYear),]
  tblSta <- tblSta[!is.na(tblSta$EndYear),]
  return(tblSta)
}


###########################################################################################
## collect location info
###########################################################################################
qStaInfo <- function(dbc,staID){
  qSta <- dbGetQuery(dbc, paste0('select * from STATIONS where STATION_NUMBER = "',staID,'"'))
  return(qSta)
}
qStaCarea <- function(dbc,staID){
  qSta <- dbGetQuery(dbc, paste0('select * from STATIONS where STATION_NUMBER = "',staID,'"'))
  return(qSta$DRAINAGE_AREA_GROSS)
}


###########################################################################################
## HYDAT temporal Query
###########################################################################################
qTemporal <- function(dbc,staID){
  qFlow <- dbGetQuery(dbc, paste0('select * from DLY_FLOWS where STATION_NUMBER = "',staID,'"'))
  DTb <- as.Date(paste0(as.numeric(qFlow[1,2]),'-',as.numeric(qFlow[1,3]),'-01'))
  DTe <- as.Date(paste0(as.numeric(tail(qFlow[2],1)),'-',as.numeric(tail(qFlow[3],1)),'-01'))
  POR <- as.numeric(DTe-DTb)
  Flow <- vector('numeric', length=POR)
  Flag <- vector('character', length=POR)
  Date <- vector('character', length=POR)
  cnt <- 0
  
  for(i in 1:nrow(qFlow)){
    yr <- qFlow[i,2]
    mo <- as.numeric(qFlow[i,3])
    
    for(d in 1:qFlow[i,5]){
      cnt <- cnt + 1
      Date[cnt] <- paste0(yr,'-',mo,'-',d)
      Flow[cnt] <- qFlow[i,(d-1)*2+12]
      Flag[cnt] <- qFlow[i,(d-1)*2+13]
    }
  }
  
  Date <- as.Date(Date)
  # anyDuplicated(Date)
  Flag[is.na(Flag)] <- ""
  hyd <- data.frame(Date,Flow,Flag)
  return(hyd[!is.na(hyd$Date),])
}