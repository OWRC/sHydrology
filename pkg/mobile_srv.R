
################################
## modify app for mobile devices
## modified from: https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
observe({
  if (input$isMobile) {
    toggle('panl')
    updateCheckboxInput(session, 'chkSW', value = TRUE)
    updateCheckboxInput(session, 'chkMet', value = TRUE)
    updateCheckboxInput(session, 'chkGW', value = FALSE)
    updateCheckboxInput(session, 'chkGWshal', value = TRUE)
    updateSliderInput(session, 'YRrng', value = c(max(tblSta$YRe),max(tblSta$YRe)))
    is.mobile <<- TRUE
  }
})