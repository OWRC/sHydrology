observe({
  if (!is.null(input$map_marker_click)){
    e <- input$map_marker_click
    sta$id <- e$id
    if (!is.null(sta$id)){
      if (is.mobile) print("mobile mode")
      if (input$chkSW) {
        if (sta$id %in% tblSta$INT_ID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblSta[tblSta$INT_ID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$typ <- 1
            sta$name <- as.character(starow$LOC_NAME)
            sta$name2 <- as.character(starow$LOC_NAME_ALT1)
            sta$hyd <- qTemporalSW(idbcsw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T) 
            sta$met <- getMeteo(starow$LAT,starow$LONG)
            if (is.na(starow$cellID)) {
              drawCarea(starow$LAT,starow$LONG)
            } else {
              drawCareaCid(starow$cellID)
            }
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",swlnk,sta$loc,"', '_blank')")
          onclick("expnd", runjs(wlnk))
        }
      }
      if (input$chkMet) {
        if (sta$id %in% tblStaMet$INT_ID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblStaMet[tblStaMet$INT_ID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$typ <- 2
            sta$name <- as.character(starow$LOC_NAME_ALT1)
            sta$name2 <- as.character(starow$LOC_NAME)
            sta$hyd <- qTemporalMET(idbcmet,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",metlnk,sta$loc,"', '_blank')")
          onclick("expnd", runjs(wlnk))
        }
      }
      if (input$chkGW | input$chkGWshal) {
        if (sta$id %in% tblGW$INT_ID) {
          withProgress(message = 'Querying..', value = 0.1, {
            starow <- tblGW[tblGW$INT_ID==sta$id,]
            sta$loc <- starow$LOC_ID
            sta$typ <- 3
            sta$name <- as.character(starow$LOC_NAME)
            sta$name2 <- as.character(starow$LOC_NAME_ALT1)
            sta$hyd <- qTemporalGW(idbcgw,sta$id)
            incProgress(0.5, detail = 'Rendering plot..')
            sta$DTb <- min(sta$hyd$Date, na.rm=T)
            sta$DTe <- max(sta$hyd$Date, na.rm=T)
            setProgress(1)
          })
          shinyjs::enable("dnld")
          shinyjs::enable("expnd")
          wlnk <- paste0("window.open('",gwlnk,sta$id,"', '_blank')")
          onclick("expnd", runjs(wlnk))          
        }
      }
    }
  }
})