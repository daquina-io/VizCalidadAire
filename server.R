rm(list=ls())

if(!require(shiny)) install.packages('shiny')
if(!require(dplyr)) install.packages('dplyr')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}
require(readr)
require(lubridate)
require(influxdbr)
require(magrittr)
require(dplyr)
##require(tibble)
##require(tidyverse)
require(purrr)

## conexión remota 
con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

## conexión localhost
## con <- influx_connection(scheme = c("http", "https"), host = "localhost",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

## dummy data to initialize dataframe and to create empty dataframe
sensorDummy <-  matrix(nrow = 0, ncol = 3)
colnames(sensorDummy) <- c("pm25", "lat", "lng")

## get data from influxdb API
db.query <- function(sensorName, time){
  x <- tryCatch({
    sensorData <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".%s WHERE time > now() - %dm GROUP BY time(2s) FILL(none) LIMIT 150",sensorName,time),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
    as.data.frame(sensorData)},
    error = function(error_message) {
      message(sprintf("error en %s",sensorName))
      return(as.data.frame(sensorDummy))
    })
}


points <- function(sensorName, time){
  df <- rbind(
    db.query(sensorName, time)
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



## -------test 
## influx_query(con = con , db = "aqa", query ="SELECT * FROM \"aqa\".\"autogen\".\"volker0016\" WHERE time > now() - 1h")

## shinyServer(function(input, output) {
##   data <- reactive({
##     as.numeric(input$integer)
## })
##   output$map <- renderLeaflet({
##     leaflet() %>%
##       addTiles() %>%
##       addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE) ) %>%
##       ##fitBounds(-74.079,4.5923,-74.065, 4.5928 ) ## la candelaria Bogota
##      fitBounds(-75.5, 6.16, -75.57, 6.35) ## medellin/test
##   })
  
##   lapply(measurements,
##          function(sensorName){
##            observe({
##              dataPoints <- points(sensorName, data())
##              invalidateLater(2000)
##              toId <- paste0(sensorName,LETTERS)
##              ## toRemoveIds <- tail(toId,n = 5  )
##              toRadious <- seq(from = 10, to = 100, by = 2) ## danger of overflow TODO
##              for( i in 1:10){
##                leafletProxy("map", data = dataPoints[i*2, ]) %>%
##                  addCircles(layerId = toId[i], ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = toRadious[i], color = ~colors,  weight = 20, label = sensorName )
##                ## FAIL attempt to erase circles
##                ## leafletProxy("map") %>%
##                 ##removeShape(toRemoveIds)
##              }
##            })
##          })
## })


### ''''''''''''''''''''' test ''''''''''''''' 
## get measurments names TODO: Must be only active sensors, how to filter that? by date?
measurements <- unlist( show_measurements(con = con,
                                          db = "aqa"
                                          ))
sensores <- paste0("aqa.autogen.",measurements,collapse=",")

## data <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s WHERE time > now() - 1m GROUP BY time(2s) FILL(none) LIMIT 1",sensores),timestamp_format = c("n", "u", "ms", "s", "m", "h")) %>% setNames(sensores) %>%  as_tibble

data <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s WHERE time > now() - 15m GROUP BY time(1m) FILL(none) LIMIT 1",sensores),timestamp_format = c("n", "u", "ms", "s", "m", "h"))


x <- as.data.frame(matrix(unlist(data), ncol=3, byrow = TRUE))

## crear una variable temporal
## map(data[[1]], function(x){
##   print("test")
##   x
## })

#data[[1]][4]
#data[[1]]$aqalacima
## se construye la trama 
#x <- as.data.frame(cbind(data[[1]][1],data[[1]][2],data[[1]][2])) ## aqui voy !! debo hacer que el dataframe traiga todos los datos
## x <- lapply(data, function(x){
##   lapply(x, function(y){
##     print(y) 
##   })
## })

## x <- lapply(data, function(x){
##   print(x)
## })
colnames(x) <- c("pm25","lat","lng")
shinyServer(function(input, output) {
  data <- reactive({
    as.numeric(input$integer)
})
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE) ) %>%
      ##fitBounds(-74.079,4.5923,-74.065, 4.5928 ) ## la candelaria Bogota
     fitBounds(-75.5, 6.16, -75.57, 6.35) ## medellin/test
  })
  leafletProxy("map", data = x )  %>%
    addCircles( ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 20, color = "green",  weight = 20, label = rownames(x) )
  })
