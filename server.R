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

## conexión remota
host <- "gblabs.co"
        ## "aqa.unloquer.org"
        ## "aireciudadano.servehttp.com"
db <-   "canairio"
        ## "aqa"
        ## "ENVdataDB"
con <- influx_connection(scheme = c("http", "https"), host = host,port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

measurements <- unlist( show_measurements(con = con,
                                          db = db
                                          ))

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
    if(host == "gblabs.co") {
        return (paste0(
            "Click para ver mediciones de la última<br/>",
            sprintf("hora del sensor <b><a target='_blank' href='%s'>%s</a></b>",link, sensorname))
            )
    }
    paste0(
        "Click para ver mediciones de la última<br/>",
        sprintf("hora del sensor <b><a target='_blank' href='http://%s:8888/sources/1/chronograf/data-explorer?query=%s'>%s</a></b>",host, query_str, sensorname)
    )
}

data <- influx_query(con, db = db, query = query(sensores(db,measurements),"1h"),timestamp_format = c("n", "u", "ms", "s", "m", "h"))

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

if(host == "gblabs.co") {
    ubicacion <- read_tsv("./canairio_sensors_mod.csv")
    x <- x %>% left_join(ubicacion, by="sensorName")
    x <- tibble(
        pm25 = x$pm25,
        sensorName = x$sensorName,
        color = x$color,
        lat = x$lat.y,
        lng = x$lng.y,
        link = x$link
    )
} else {
    x <- tibble(
        pm25 = x$pm25,
        sensorName = x$sensorName,
        color = x$color,
        lat = x$lat,
        lng = x$lng
    )
}

x <- unique(x)
x <- x[!is.na(x$lng),]

## No funciona encuadre
encuadre <- function(servidor) {
    if(servidor != "gblabs.co") return(fitBounds(-74.079,4.46,-74.065, 4.823))##Bogotá
    fitBounds(-75.5, 6.16, -75.57, 6.35)#Medellín
}

## write_tsv(x,"/tmp/aireciudadano.txt")
shinyServer(function(input, output) {
  data <- reactive({
   #as.numeric(input$integer)
  })

  output$map <- renderLeaflet({
      leaflet() %>%
          ## addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE) ) %>%
          addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(noWrap = TRUE) ) %>%
          fitBounds(-74.079,4.46,-74.065, 4.823) ## Bogotá
          ## fitBounds(-75.5, 6.16, -75.57, 6.35) ## Medellin
  })

  leafletProxy("map", data = x )  %>%
      addCircles( ~as.numeric(lng), ~as.numeric(lat),
                 popup = ~popup_content(query(sensores_ts(db, x$sensorName)), x$sensorName, x$link),
                 opacity= 0.9, fillOpacity = 0.9, radius = 30, fillColor= ~color, color = ~color,  weight = 30, label = ~as.character(as.integer(pm25)), labelOptions = labelOptions(noHide = TRUE, offset=c(0,22), textOnly = TRUE, direction = "top", style = list("color" = "white", "font-weight" = "bold", "font-size" = "12px")))
})
