rm(list=ls())

if(!require(tidyverse)) install.packages('tidyverse')
if(!require(shiny)) install.packages('shiny')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}
if(!require(lubridate)) install.packages('lubridate')
if(!require(influxdbr)) install.packages('influxdbr')

## conexión remota
host <- ## "gblabs.co"
        "aqa.unloquer.org"
        ## "aireciudadano.servehttp.com"
db <-   ## "canairio"
        "aqa"
        ## "ENVdataDB"
con <- influx_connection(scheme = c("http", "https"), host = host,port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

## dummy data to initialize dataframe and to create empty dataframe
sensorDummy <-  matrix(nrow = 0, ncol = 3)
colnames(sensorDummy) <- c("pm25", "lat", "lng")

## get data from influxdb API
db.query <- function(sensorName, time){
  x <- tryCatch({
    sensorData <- influx_query(con, db = db, query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s.\"autogen\".\"\"%s\"\" WHERE time > now() - %dm GROUP BY time(2s) FILL(none) LIMIT 150",db,sensorName,time),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
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

measurements <- unlist( show_measurements(con = con,
                                          db = db
                                          ))

sensores <- paste0("\"",db,"\"",".autogen.","\"",measurements,"\"",collapse=",")


data <- influx_query(con, db = db, query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s WHERE time > now() - 1h GROUP BY time(1h) FILL(none) LIMIT 1",sensores),timestamp_format = c("n", "u", "ms", "s", "m", "h"))


x <- as.data.frame(matrix(unlist(data), ncol=3, byrow = TRUE))

## names
colnames(x) <- c("pm25","lat","lng")

## add sensor name from data structure
x$sensorName <-  unique(names(data[[1]]))

## add ICApm25 colors
x$color <- (lapply(x$pm25, function(x)(
  ifelse(x < 12 , "green",
  ifelse(x < 35 && x >= 12 , "gold",
  ifelse( x < 55 && x >= 35, "orange",
  ifelse( x < 150 && x >= 55, "red",
  ifelse( x < 250 && x >= 150, "purple",
         "maroon"))))))) %>% enframe %>% unnest)$value

ubicacion <- read_tsv("./canairio_sensors_mod.csv")

x <- tibble(
    pm25 = x$pm25,
    sensorName = x$sensorName,
    color = x$color
)

x <- x %>% left_join(ubicacion, by="sensorName")

## write_tsv(data.frame(sensor=x$sensorName),"/tmp/aireciudadano.txt")
shinyServer(function(input, output) {
  data <- reactive({
   #as.numeric(input$integer)
})
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE) ) %>%
        fitBounds(-74.079,4.46,-74.065, 4.823) ## la candelaria Bogota
     ## fitBounds(-75.5, 6.16, -75.57, 6.35) ## medellin/test
  })
  leafletProxy("map", data = x )  %>%
    addCircles( ~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 20, color = ~color,  weight = 20, label = ~sensorName)
  })
