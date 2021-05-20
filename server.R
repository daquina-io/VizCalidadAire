rm(list=ls())

if(!require(tidyverse)) install.packages('tidyverse')
if(!require(shiny)) install.packages('shiny')
if(!require(leaflet)) {
    devtools::install_github('rstudio/leaflet')
    devtools::install_github("rstudio/leaflet.providers")
    devtools::install_github('bhaskarvk/leaflet.extras')
}
if(!require(lubridate)) install.packages('lubridate')
if(!require(influxdbr)) install.packages('influxdbr')

## Sensor meta data
sensors_data <- read_tsv("./canairio_sensors_mod.csv")

## conexión remota, implementación para consultar múltiples servidores
## hosts <- unique(sensors_data$server)
hosts <- "influxdb.canair.io"
db <-   "canairio"

cons <- map(hosts, function(h) {
  influx_connection(scheme = c("http", "https"), host = h, port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")
}) %>% set_names(hosts)

sensores <- function(db, measurements) {
    paste0("\"",db,"\"",".autogen.","\"",measurements,"\"",collapse=",")
}

sensores_ts <- function(db, measurements) {
    map(measurements, function(ms) paste0("\"",db,"\"",".autogen.","\"",ms,"\"",collapse=","))
}

## "aqa"."autogen"."volker0004"
query_time_series <- "SELECT mean(\"pm25\") AS \"pm25\" FROM %s WHERE time > now() - 1h GROUP BY time(10s) FILL(none)"
query_1h_mean <- "SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM %s WHERE time > now() - 1h GROUP BY time(1h) FILL(none) LIMIT 1"

query <- function(snsrs, type = "ts") {
    if(type == "1h") sprintf(query_1h_mean, snsrs)
    else sprintf(query_time_series, snsrs)
}

popup_content <- function(query_str, sensorname, link = "ada") {
  return (paste0(
            "Click para ver mediciones de la última<br/>",
            sprintf("hora del sensor <b><a target='_blank' href='%s'>%s</a></b>",link, sensorname))
          )
}

datas <- map(hosts, function(h){
  influx_query(
    cons[[h]],
    db = db,
    query = query(sensores(db, sensors_data[sensors_data$server == h,"sensorName"][[1]]),"1h"),
    timestamp_format = c("n", "u", "ms", "s", "m", "h"),
    return_xts = FALSE)[[1]]
}) %>% enframe %>% unnest(cols = c(value))

## add ICApm25 colors
datas$color <- (lapply(datas$pm25, function(x)(
  ifelse(x < 12 , "green",
  ifelse(x < 35 && x >= 12 , "gold",
  ifelse( x < 55 && x >= 35, "orange",
  ifelse( x < 150 && x >= 55, "red",
  ifelse( x < 250 && x >= 150, "purple",
         "maroon"))))))) %>% enframe %>% unnest(cols = c(value)))$value

datas <- datas %>% left_join(sensors_data, by=c("series_names"="sensorName"))
datas <- tibble(
  pm25 = datas$pm25*datas$pendiente + datas$intercepto,
  series_names = datas$series_names,
  color = datas$color,
  lat = datas$lat.y,
  lng = datas$lng.y,
  link = datas$link
)

## No funciona encuadre
encuadre <- function(servidor) {
    if(servidor != "gblabs.co") return(fitBounds(-74.079,4.46,-74.065, 4.823))##Bogotá
    fitBounds(-75.5, 6.16, -75.57, 6.35)#Medellín
}

## write_tsv(x,"/tmp/aireciudadano.txt")
shinyServer(function(input, output) {
  ## data <- reactive({
  ##  #as.numeric(input$integer)
  ## })

  output$map <- renderLeaflet({
      leaflet() %>%
          ## addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE) ) %>%
          addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(noWrap = TRUE) ) %>%
          fitBounds(-74.079,4.46,-74.065, 4.823) ## Bogotá
          ## fitBounds(-75.5, 6.16, -75.57, 6.35) ## Medellin
  })

  leafletProxy("map", data = datas )  %>%
      addCircles( ~as.numeric(lng), ~as.numeric(lat),
                 popup = ~popup_content(query(sensores_ts(db, datas$series_names)), datas$series_names, datas$link),
                 opacity= 0.9, fillOpacity = 0.9, radius = 30, fillColor= ~color, color = ~color,  weight = 30, label = ~as.character(as.integer(pm25)), labelOptions = labelOptions(noHide = TRUE, offset=c(0,22), textOnly = TRUE, direction = "top", style = list("color" = "white", "font-weight" = "bold", "font-size" = "12px")))
})
