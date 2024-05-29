# Dygraph hydrograph preview in absolutePanel
output$hydgrph <- renderDygraph({
  if (!is.null(sta$hyd)){
    switch(sta$typ,
           { # 1=surface water
             hydr <- sta$hyd[sta$hyd$Flow>0,]
             qFlw <- xts(hydr$Flow, order.by = hydr$Date)
             lw <- max(20,25 + (log10(max(hydr$Flow))-2)*8) # dynamic plot fitting
             if( is.null(sta$met) ) {
               qxts <- qFlw
               colnames(qxts) <- 'Discharge'
               dygraph(qxts) %>%
                 dySeries("Discharge", color = "blue") %>%
                 # dyLegend(show = 'always') %>%
                 dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
                 dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
                 dyRangeSelector(strokeColor = '') %>% 
                 dyLegend(labelsDiv = "legendDivID")              
             } else {
               df2 <- hydr %>% inner_join(sta$met, by="Date")
               qRf <- xts(df2$Rf, order.by = df2$Date)
               qSm <- xts(df2$Sm, order.by = df2$Date)
               qxts <- cbind(qFlw,qRf,qSm)
               colnames(qxts) <- c('Discharge','Rainfall','Snowmelt')
               dygraph(qxts) %>%
                 dySeries("Discharge", color = "blue") %>%
                 dyBarSeries("Rainfall", axis = 'y2', color="#1f78b4") %>%
                 dyBarSeries("Snowmelt", axis = 'y2', color="#a6cee3") %>%
                 dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
                 dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
                 dyAxis('y2', labelWidth=0, axisLabelWidth=0, valueRange = c(100, 0)) %>%
                 dyRangeSelector(strokeColor = '') %>% 
                 dyLegend(labelsDiv = "legendDivID")
             }
           },
           { # 2=climate
             qxts <- xts(cbind(sta$hyd$precipitation_amount,sta$hyd$mean_air_temperature), order.by = sta$hyd$Date)
             lw <- max(20,25 + (log10(max(sta$hyd$precipitation_amount))-2)*8) # dynamic plot fitting
             colnames(qxts) <- c('precipitation_amount','mean_air_temperature')
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, stepPlot = TRUE) %>%
               dyAxis(name='y', labelWidth=0, axisLabelWidth=lw) %>%
               dyRangeSelector(strokeColor = '') %>% 
               dyLegend(labelsDiv = "legendDivID")
           },
           { # 3=gw
             h1 <- sta$hyd[year(sta$hyd$Date)>1900,]
             qxts <- xts(h1, order.by = h1$Date)
             dygraph(qxts) %>%
               # dyLegend(show = 'always') %>%
               dyOptions(axisLineWidth = 1.5, fillGraph = TRUE) %>%
               dyAxis(name='y', labelWidth=0) %>%
               dyRangeSelector(strokeColor = '') %>% 
               dyLegend(labelsDiv = "legendDivID")
           })
  }
})