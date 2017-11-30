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

## ## leer de csv
## points <- read_csv("https://github.com/daquina-io/VizCalidadAire/raw/master/data/points.csv")
## points <- points[points$lat != "INVALID",]
## points <- points[points$lng != "INVALID",]
## points <- points[as.numeric(points$lng) < -70,]
## points <- points[!is.na(points$lat),]
## points$date_hour <- mdy_hms(paste0(points$date," ",points$hour))
## points$date_hour <- points$date_hour - hours(5)
## points$hour<-  hms(points$hour)

## ## intervalos fechas
## intervalo_fechas <- function(start_end) { (interval(start_end[1],start_end[2])) }
## intervalo_horas <- function(start_end) { (hms(start_end[1],start_end[2])) }

## x <- intervalo_horas(c((points$hour[1]),(points$hour[1]+hours(1))))
## hours(5)

## read from influx
con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

points <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"pm25\", mean(\"lat\") AS \"lat\", mean(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".\"volker0002\" WHERE time > now() - 30d GROUP BY time(10s)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
points2 <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"pm25\", mean(\"lat\") AS \"lat\", mean(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".\"volker0001\" WHERE time > now() - 30d GROUP BY time(10s)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
points3 <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"pm25\", mean(\"lat\") AS \"lat\", mean(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".\"volker0003\" WHERE time > now() - 30d GROUP BY time(10s)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
points4 <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"pm25\", mean(\"lat\") AS \"lat\", mean(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".\"volkerC3p\" WHERE time > now() - 30d GROUP BY time(10s)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
points <- as.data.frame(points)
points2 <- as.data.frame(points2)
points3 <- as.data.frame(points3)
points4 <- as.data.frame(points4)
points <- rbind(points, points2, points3, points4 )

## quita NA's'
points[complete.cases(points), ]

points$date_hour <- rownames(points)




## exclude values > 500
points <- points[points$pm25 < 500,]

## colors
points$colors <- lapply(points$pm25, function(x)(
  ifelse(x < 12 , "green",
    ifelse(x < 35 && x >= 12 , "orange",
      ifelse( x < 55 && x >= 35, "red","purple")))
))

shinyServer(function(input, output) {
  data <- reactive({
    points %>% filter( pm25 >= input$range[1], pm25 <= input$range[2] )  -> filtered_points
    filtered_points
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
       fitBounds(-75.5, 6.2, -75.57, 6.28)
  })

  observe({
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      ## addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = ~as.numeric(pm25)) ## no colors
      ## addCircles(~as.numeric(lng), ~as.numeric(lat), popup = paste("PM2.5:",points$pm25," -- ","Fecha:",points$date,points$hour), fillOpacity = 0.7, radius = 10, color = ~colors)
      addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = 15, color = ~colors)
  })
})
