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
    sensorData <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM \"aqaTest\".\"autogen\".%s WHERE time > now() - 10m GROUP BY time(1s) FILL(none) LIMIT 260",sensorName),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
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
  data <- reactive({
    integer(input$integer)  
})
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE) ) %>%
      fitBounds(-75.5, 6.16, -75.57, 6.35)
  })

  numberOfLectures <- 25 ## TODO reactive with a slide in UI
  lapply(paste0("V",as.character(c(0:3))),
         function(sensorName){
           observe({
             dataPoints <- points(sensorName)
             invalidateLater(2000)
             toId <- paste0(sensorName,LETTERS)
             toRadious <- seq(from = 10, to = 100, by = 2) ## danger of overflow TODO
             for( i in 1:length(data())){
               leafletProxy("map", data = dataPoints[i*5, ]) %>%
                 addCircles(layerId = toId[i], ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = toRadious[i], color = ~colors,  weight = 5, label = sensorName )
             }
           })
         }
         )
})


