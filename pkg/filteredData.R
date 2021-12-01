

filteredDataSW <- reactive({
  p <- as.numeric(input$POR)*365.25
  tblSta[tblSta$YRe >= input$YRrng[1] & tblSta$YRb <= input$YRrng[2] & tblSta$CNT > p,]
})

filteredDataMet <- reactive({
  p <- as.numeric(input$POR)*365.25
  tblStaMet[tblStaMet$YRe >= input$YRrng[1] & tblStaMet$YRb <= input$YRrng[2] & tblStaMet$PCNT > p,]   
})

filteredDataGW <- reactive({
  p <- as.numeric(input$POR)
  tblGW[tblGW$YRe >= input$YRrng[1] & tblGW$YRb <= input$YRrng[2] & tblGW$SCREEN_TOP_DEPTH_M > 20 & (tblGW$YRe-tblGW$YRb) > p,]
})

filteredDataGWshallow <- reactive({
  p <- as.numeric(input$POR)
  tblGW[tblGW$YRe >= input$YRrng[1] & tblGW$YRb <= input$YRrng[2] & tblGW$SCREEN_TOP_DEPTH_M < 20 & tblGW$SCREEN_TOP_DEPTH_M >= 0 & (tblGW$YRe-tblGW$YRb) > p,]
})

