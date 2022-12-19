# sHydrologyMap
A Shiny-Leaflet interface and data extractor for WSC HYDAT database.

Currently built to view WSC HYDAT stream flow data [click here](https://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1).

Can be used in conjunction with [sHydrology analysis tools](https://github.com/maseology/sHydrology_analysis).

### Current functionality:
 * Select gauge from a map
	 * filter locations based on period of record
 * Preview entire stream flow time-series
 * Dynamic hydrograph zooming:
	 * drag-and-click zoom
	 * double-click to full extent
	 * optionnally use range slider 
 * Export data as *.csv 

### Screenshot:
![Screenshot](/images/screenshot.png)


## Instructions:
As coded, the *sHydrology.R* Shiny-Leaflet application is built to view the Water Survey of Canada (WSC) **HY**drological **DAT**abase (HYDAT) which can be downloaded [here](https://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1). *sHydrology.R* will preview all of Canada, but can be defaulted to your home province.

Download and extract the SQLite format of the database typically compressed as *'Hydat_sqlite3_YYYYMMDD.zip'*, where *'YYYYMMDD'* is the date of release. Extract the SQLite file *'Hydat.sqlite3'* and place database anywhere on your local machine.

Using [RStudio](https://https://www.rstudio.com/), install the required packages (see below), and insert the path of the database has to be placed on *Line 22* of the main app file: *sHydrology.R*. 

Run *sHydrology.R* externally such that the map will open on your default web browser. (The app must be run externally in order to extract *.csv files.)

The *'Hydat.sqlite3'* file is roughly 1GB in size and thus cannot be hosted on GitHub.

### Required R-dependent packages:
 * leaflet
 * shiny
 * shinyjs
 * jsonlite
 * shinyBS
 * dygraphs
 * RSQLite
 * zoo
 * xts

### License

sHydrology hosted on GitHub is released under the MIT license.

### Contributors

Mason Marchildon P.Eng M.A.Sc, Hydrologist for the [Oak Ridges Moraine Groundwater Program](https://github.com/OWRC)
