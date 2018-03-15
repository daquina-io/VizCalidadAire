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
      sensor <- influx_query(con, db = "aqa", query = sprintf("SELECT mean(\"pm25\") AS \"pm25\", median(\"lat\") AS \"lat\", median(\"lng\") AS \"lng\" FROM \"aqa\".\"autogen\".%s WHERE time > nhttp://192.168.4.1/wifisave?s=HOME-CAA1&p=94A1D68736EB7CD5ow() - 1h GROUP BY time(1h) FILL(none) LIMIT 1",sensor),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
      as.data.frame(sensor)}
  ,  error = function(error_message) {
    message(sprintf("error en %s",sensor))
    return(as.data.frame(sensorDummy))
  })
}

points <- rbind(
  db.query("volker0002"),
  db.query("volker0003"),
  db.query("volker0004"),
  db.query("volker0005"),
  db.query("volker0006"),
  db.query("volker0007"),
  db.query("volker0008"),
  db.query("volker0009"),
  db.query("volker0010"),
  db.query("volker0011"),
  db.query("volker0012"),
  db.query("volker0013"),
  db.query("volker0014"),
 # db.query("volker0015"),
 # db.query("volker0016"),
 # db.query("volker0017"),
  db.query("volkerC3p"),
  db.query("valenciasanchez"),
  db.query("florida_nueva")
)



## ========= otra manera

## points <- influx_select(con = con, 
##                         db = "aqa", 
##                         field_keys = "pm25,lat,lng", 
##                         measurement = "volker0008",
##                         where = "time > now() - 1h",
##                         group_by = "*",
##                         limit = 10,
##                         order_desc = TRUE, 
##                         return_xts = TRUE)

## ## quita NA's'
## points[complete.cases(points), ]
## points$date_hour <- rownames(points)

## ## exclude values > 500
## points <- points[points$pm25 < 500,]




## colors
points$colors <- lapply(points$pm25, function(x)(
  ifelse(x < 12 , "green",
  ifelse(x < 35 && x >= 12 , "gold",
  ifelse( x < 55 && x >= 35, "orange",
  ifelse( x < 150 && x >= 55, "red",
  ifelse( x < 250 && x >= 150, "purple",
           "maroon")))))
))

shinyServer(function(input, output) {
  data <- reactive({
    points %>% filter( pm25 >= input$range[1], pm25 <= input$range[2] )  -> filtered_points
    filtered_points
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE) ) %>%
      fitBounds(-75.5, 6.16, -75.57, 6.35)


  })

  observe({
    leafletProxy("map", data = data()) %>%
      clearShapes() %>%
      ## addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.7, radius = ~as.numeric(pm25)) ## no colors
      ## addCircles(~as.numeric(lng), ~as.numeric(lat), popup = paste("PM2.5:",points$pm25," -- ","Fecha:",points$date,points$hour), fillOpacity = 0.7, radius = 10, color = ~colors)
      addCircles(~as.numeric(lng), ~as.numeric(lat), popup = ~as.character(pm25), fillOpacity = 0.9, radius = 100, color = ~colors,  weight = 5)
  })
})
