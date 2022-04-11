# Dygraph hydrograph preview in absolutePanel
output$hydgrph <- renderDygraph({
  if (!is.null(sta$hyd)){
    switch(sta$typ,
           { # 1=surface water
             qxts <- xts(sta$hyd$Flow, order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$Flow))-2)*8) # dynamic plot fitting
             colnames(qxts) <- 'Discharge'
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
               dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
               dyRangeSelector(strokeColor = '')                 
           },
           { # 2=climate
             qxts <- xts(cbind(sta$hyd$precipitation_amount,sta$hyd$mean_air_temperature), order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$precipitation_amount))-2)*8) # dynamic plot fitting
             colnames(qxts) <- c('precipitation_amount','mean_air_temperature')
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
               dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
               dyRangeSelector(strokeColor = '')  
           },
           { # 3=gw
             h1 <- sta$hyd[year(sta$hyd$Date)>1900,]
             qxts <- xts(h1, order.by = h1$Date)
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE) %>%
               dyAxis(name='y', labelWidth=0) %>%
               dyRangeSelector(strokeColor = '')  
           })
  }
})