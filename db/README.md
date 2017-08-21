# sHydrology

As coded, the sHydrology.R Shiny-Leaflet application is built to view the Water Survey of Canada (WSC) **HY**drological **DAT**abase (HYDAT) which can be downloaded [here](https://www.ec.gc.ca/rhc-wsc/default.asp?lang=En&n=9018B5EC-1). sHydrology.R is defaulted to Ontario gauges only.

Download and extract the SQLite format of the database typically compressed as *'Hydat_sqlite3_YYYYMMDD.zip'*, where *'YYYYMMDD'* is the date of release. Extract the SQLite file *'Hydat.sqlite3'* and place in the */db/* directory.

*'Hydat.sqlite3'* is roughly 1GB in size and thus cannot be hosted on GitHub.