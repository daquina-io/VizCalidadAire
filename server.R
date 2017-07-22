if(!require(shiny)) install.packages('shiny')
if(!require(dplyr)) install.packages('dplyr')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}
require(readr)
require(lubridate)
##library(RColorBrewer)

points <- read_csv("./data/points.csv")
points <- points[points$lat != "INVALID",]
points <- points[points$lng != "INVALID",]
points <- points[as.numeric(points$lng) < -70,]
points <- points[!is.na(points$lat),]
points$date_hour <- mdy_hms(paste0(points$date," ",points$hour))
points$date_hour <- points$date_hour - hours(5)

## intervalos fechas
intervalo_fechas <- function(start_end) { (interval(start_end[1],start_end[2])) }

## exclude values > 500
points <- points[points$pm25 < 500,]

## colors
points$colors <- lapply(points$pm25, function(x)(
  ifelse(x < 30 , "green",
    ifelse(x < 50, "yellow",
      ifelse( x < 100, "red","purple")))
))

shinyServer(function(input, output) {
  data <- reactive({
    points %>% filter( wday(date_hour, label=TRUE, abbr=FALSE) %in% input$wday, pm25 >= input$range[1], pm25 <= input$range[2], date_hour %within% intervalo_fechas(input$dates))  -> filtered_points
    filtered_points
  })

  output$map <- renderLeaflet({
    points <- data()
    leaflet(data = points) %>%
      addTiles() %>%
      ## addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = ~as.numeric(pm25)) ## no colors
      addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = 10, color = ~colors)
  })
})
