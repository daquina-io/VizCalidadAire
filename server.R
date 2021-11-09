rm(list = ls())

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(shiny)) install.packages("shiny")
if (!require(leaflet)) devtools::install_github("rstudio/leaflet")
if (!require(lubridate)) install.packages("lubridate")
if (!require(influxdbr)) install.packages("influxdbr")
if (!require(geohashTools)) install.packages("geohashTools")
library(xts)
library(dygraphs)


## conexión remota
host <- "influxdb.canair.io"
db <- "canairio"

con <- influx_connection(scheme = c("http", "https"), host = host, port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

measurements <- unlist(show_measurements(
  con = con,
  db = db
))

sensores <- function(db, measurements) {
  paste0("\"", db, "\"", ".autogen.", "\"", measurements, "\"", collapse = ",")
}

sensores_ts <- function(db, measurements) {
  map(measurements, function(ms) paste0("\"", db, "\"", ".autogen.", "\"", ms, "\"", collapse = ","))
}

## "aqa"."autogen"."volker0004"
query_time_series <- "SELECT mean(\"pm25\") AS \"pm25\" FROM %s WHERE time > now() - 1h GROUP BY time(1m) FILL(none)"
## query_stas <- "SELECT last(\"name\") AS \"name\" FROM \"fixed_stations_01\" WHERE time > now() - 1h GROUP BY time(1h), \"mac\" fill(none) LIMIT 1"
## query_1h_mean <- "SELECT median(\"pm25\") AS \"pm25\" FROM \"fixed_stations_01\" WHERE time > now() - 1h GROUP BY time(1m), \"name\" fill(none)"
query_n <- "SELECT median(\"pm25\") AS \"pm25\", last(\"name\") AS \"name\", last(\"geo\") AS \"geohash\" FROM \"fixed_stations_01\" WHERE time > now() - 1h GROUP BY time(1h), \"mac\" fill(none) LIMIT 1"

query <- function(snsrs, type = "ts") {
  if (type == "1h") {
    sprintf(query_1h_mean, snsrs)
  } else {
    sprintf(query_time_series, snsrs)
  }
}

popup_content <- function(query_str, sensorname, link = "ada") {
  if (host == "gblabs.co") {
    return(paste0(
      "Click para ver mediciones de la última<br/>",
      sprintf("hora del sensor <b><a target='_blank' href='%s'>%s</a></b>", link, sensorname)
    ))
  }
  paste0(
    "Click para ver mediciones de la última<br/>",
    sprintf("hora del sensor <b><a target='_blank' href='http://%s:8888/sources/1/chronograf/data-explorer?query=%s'>%s</a></b>", host, query_str, sensorname)
  )
}

## data <- influx_query(con, db = db, query = query(sensores(db, measurements), "1h"), timestamp_format = c("n", "u", "ms", "s", "m", "h"), return_xts = FALSE)[[1]]
data <- influx_query(con, db = db, query = query_n, timestamp_format = c("n", "u", "ms", "s", "m", "h"), return_xts = FALSE)[[1]]
## data_stas <- influx_query(con, db = db, query = query_stas, timestamp_format = c("n", "u", "ms", "s", "m", "h"), return_xts = FALSE)[[1]]["name"]
## data_1h <- influx_query(con, db = db, query = query_1h_mean, timestamp_format = c("n", "u", "ms", "s", "m", "h"), return_xts = TRUE)


glimpse(data_1h)

data[, c("latitude", "longitude")] <- data$geohash %>% gh_decode()

## add ICApm25 colors
data$color <- (lapply(data$pm25, function(x) {
  (
    ifelse(x < 12, "green",
      ifelse(x < 35 && x >= 12, "gold",
        ifelse(x < 55 && x >= 35, "orange",
          ifelse(x < 150 && x >= 55, "red",
            ifelse(x < 250 && x >= 150, "purple",
              "maroon"
            )
          )
        )
      )
  ))
}) %>% enframe() %>% unnest())$value

encuadre <- function(ciudad) {
  if (ciudad == "bogota") {
    return(c(-74.079, 4.46, -74.065, 4.823))
  } else if (ciudad == "medellin") {
    return(c(-75.5, 6.16, -75.57, 6.35))
  } else if (ciudad == "cali") {
    return(c(-76.7103, 3.5676, -75.57, 3.3225))
  } else if (ciudad == "lima") {
    return(c(-77.3685, -11.8243, -76.3361, -12.3044))
  } else if (ciudad == "bilbao") {
    return(c(-3.2918, 43.4449, -2.5433, 43.0874))
  } else if (ciudad == "berlin") {
    return(c(13.0497, 52.6601, 13.7981, 52.3613))
  } else if (ciudad == "global") {
    return(c(-177.2, 82.1, 206.0, -69.7))
  } else if (ciudad == "costa") {
    return(c(-76.970, 11.760, -70.983, 7.891))
  }
}

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet(data) %>%
      ## addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.Terrain, options = providerTileOptions(noWrap = TRUE)) %>%
      addCircles(~ as.numeric(longitude), ~ as.numeric(latitude),
        popup = ~ popup_content(query(sensores_ts(db, data$name)), data$name, data$link),
        opacity = 0.9, fillOpacity = 0.9, radius = 20, fillColor = ~color, color = ~color, weight = 20, label = ~ as.character(as.integer(pm25)), labelOptions = labelOptions(noHide = TRUE, offset = c(0, 22), textOnly = TRUE, direction = "top", style = list("color" = "white", "font-weight" = "bold", "font-size" = "12px"))
      )
  })

  ## output$dygraph <- renderDygraph({
  ##   dygraph(data_1h$P25, order.by = data_1h$fixed_stations_01), main = "Exposición PM25", ylab = "μg/m³") %>%
  ##     dySeries(label = "μg/m³") %>%
  ##     dyRangeSelector()
  ## })

  observe({
    leafletProxy("map", data = data) %>%
      fitBounds(encuadre(input$ciudad)[1], encuadre(input$ciudad)[2], encuadre(input$ciudad)[3], encuadre(input$ciudad)[4])
  })
})
