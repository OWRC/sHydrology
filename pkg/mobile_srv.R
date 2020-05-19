
################################
## modify app for mobile devices
## modified from: https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
observe({
  if (input$isMobile) {
    toggle('panl')
    updateCheckboxInput(session, 'chkMet', value = TRUE)
  }
})