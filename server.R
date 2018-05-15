if(!require(shiny)) install.packages('shiny')
if(!require(dplyr)) install.packages('dplyr')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}
require(readr)
require(lubridate)
require(influxdbr)
##library(RColorBrewer)

## read from influx
con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

sensorDummy <-  matrix(nrow = 0, ncol = 3)
colnames(sensorDummy) <- c("pm25", "lat", "lng")

db.query <- function(sensor){
    x <- tryCatch({
      sensor <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".%s WHERE time > now() - 1h GROUP BY time(1h) FILL(none) LIMIT 1",sensor),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
      as.data.frame(sensor)},
      error = function(error_message) {
        message(sprintf("error en %s",sensor))
        return(as.data.frame(sensorDummy))
  })
}

## get measurments names
measurements <- unlist(show_measurements(con = con,
                                 db = "aqa"
                                 ))

points <- function(sensorName){
  df <- rbind(
    db.query(sensorName)
  )
  df$colors <- lapply(df$pm25, function(x)(
    ifelse(x < 12 , "green",
    ifelse(x < 35 && x >= 12 , "gold",
    ifelse( x < 55 && x >= 35, "orange",
    ifelse( x < 150 && x >= 55, "red",
    ifelse( x < 250 && x >= 150, "purple",
           "maroon")))))
  ))
   return(df)
}

shinyServer(function(input, output) {
  data <- reactive({
   ## points %>% filter( pm25 >= input$range[1], pm25 <= input$range[2] )  -> filtered_points
   ##filtered_points
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE) ) %>%
      fitBounds(-75.5, 6.16, -75.57, 6.35)
  })

   lapply(measurements, 
         function(sensorName){
           observe({
             dataPoints <- points(sensorName)
             invalidateLater(4000)
             toId <- paste0(sensorName,LETTERS)
             ## toRemoveIds <- tail(toId,n = 5  )
             toRadious <- seq(from = 10, to = 100, by = 2) ## danger of overflow TODO
             for( i in 1:length(data())){
               leafletProxy("map", data = dataPoints[i*5, ]) %>%
                 addCircles(layerId = toId[i], ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = toRadious[i], color = ~colors,  weight = 5, label = sensorName )
               ## FAIL attempt to erase circles
               ## leafletProxy("map") %>%  
               ## removeShape(toRemoveIds)
             }
           })
         })
})




