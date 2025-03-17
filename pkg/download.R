

output$dnld <- downloadHandler(
  filename <- function() { paste0(sta$name, '.csv') },
  content <- function(file) {
    switch(sta$typ,
           { # 1=surface water
             if(!is.null(sta$hyd)) {
               if( is.null(sta$met) ) {
                 write.csv(sta$hyd[!is.na(sta$hyd$Flow),], file, row.names = FALSE)
               } else {
                 df <- sta$hyd %>% inner_join(sta$met, by="Date")
                 write.csv(df[!is.na(df$Flow),], file, row.names = FALSE)
               }
             } 
           },
           { # 2=climate
             if(!is.null(sta$hyd)) write.csv(sta$hyd[colSums(!is.na(sta$hyd)) > 0], file, row.names = FALSE)
           },
           { # 3=gw monitoring
             if(!is.null(sta$hyd)) write.csv(sta$hyd, file, row.names = FALSE)
           })
  } 
)


output$dnldCA <- downloadHandler(
  filename <- function() { paste0(sta$name, '.geojson') },
  content <- function(file) {
    print('here')
    if(!is.null(sta$ca)) write(sta$ca, file)
  } 
)

