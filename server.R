if(!require(shiny)) install.packages('shiny')
if(!require(dplyr)) install.packages('dplyr')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}
require(readr)
require(lubridate)
require(influxdbr)

## read from influx
con <- influx_connection(scheme = c("http", "https"), host = "localhost",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

## dummy data to initialize dataframe and to create empty dataframe
sensorDummy <-  matrix(nrow = 0, ncol = 3)
colnames(sensorDummy) <- c("pm25", "lat", "lng")

## get data from influxdb API
db.query <- function(sensorName){
  x <- tryCatch({
    sensorData <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM \"aqaTest\".\"autogen\".%s WHERE time > now() - 30s GROUP BY time(1s) FILL(none) LIMIT 20",sensorName),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
    as.data.frame(sensorData)},
    error = function(error_message) {
      message(sprintf("error en %s",sensorName))
      return(as.data.frame(sensorDummy))
    })
}

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
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE) ) %>%
      fitBounds(-75.5, 6.16, -75.57, 6.35)
  })
  draw <- function(sensorName){
    observe({
      invalidateLater(1000)
      dataPoints <- points(sensorName)
       leafletProxy("map", data = dataPoints[19, ]) %>%
         addCircles(layerId = paste0(sensorName,"A"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 50, color = ~colors,  weight = 5)
       leafletProxy("map", data = dataPoints[15, ]) %>%
         addCircles(layerId = paste0(sensorName,"B"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 40, color = ~colors,  weight = 5)
       leafletProxy("map", data = dataPoints[13, ]) %>%
        addCircles(layerId = paste0(sensorName,"C"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 30, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[9, ]) %>%
        addCircles(layerId = paste0(sensorName,"D"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 20, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[5, ]) %>%
        addCircles(layerId = paste0(sensorName,"E"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = 10, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[1, ]) %>%
        addCircles(layerId = paste0(sensorName,"F"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.5, radius = 5, color = ~colors,  weight = 5)
    })
  }
  ## can construct names with as.character((c:10))
  # draw("v")
  ## draw("002")
  ## draw("volkerX002")
  lapply(paste0("V",as.character(c(0:10))),
 function(sensorName){
    observe({
      invalidateLater(1000)
      dataPoints <- points(sensorName)
       leafletProxy("map", data = dataPoints[19, ]) %>%
         addCircles(layerId = paste0(sensorName,"A"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 50, color = ~colors,  weight = 5, label = sensorName )
       leafletProxy("map", data = dataPoints[13, ]) %>%
         addCircles(layerId = paste0(sensorName,"B"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 20, color = ~colors,  weight = 5)
       leafletProxy("map", data = dataPoints[10, ]) %>%
        addCircles(layerId = paste0(sensorName,"C"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 15, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[7, ]) %>%
        addCircles(layerId = paste0(sensorName,"D"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 10, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[5, ]) %>%
        addCircles(layerId = paste0(sensorName,"E"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = 5, color = ~colors,  weight = 5)
      leafletProxy("map", data = dataPoints[1, ]) %>%
        addCircles(layerId = paste0(sensorName,"F"), ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.5, radius = 3, color = ~colors,  weight = 5)
    })
  }
 )
})


