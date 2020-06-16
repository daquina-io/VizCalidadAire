library(plotly)
library(tidyverse)
library(jsonlite)
library(httr)
library(leaflet)
library(lubridate)

dataStaHelena <- read.csv("~/Data/estacion_data_calidadaire_94_20200201_20200229.csv", header = FALSE)
dataCentro <- read.csv("~/Data/estacion_data_calidadaire_12_20200201_20200229.csv", header = FALSE)

dataStaHelena <- read.csv("~/Data/openaq_centro.csv", header = FALSE)
dataCentro <- read.csv("~/Data/openaq_laY.csv", header = FALSE)

dataStaHelena <- dataStaHelena %>% filter(!(V6 %in% c("o3","pm10")))   %>% filter(!(V7 == "-9999")) 
dataCentro <- dataCentro %>% filter(!(V6 %in% c("o3","pm10")))%>% filter(!(V7 == "-9999"))
dataStaHelena <- dataStaHelena[-1,] #quita primera fila
dataCentro <- dataCentro[-1,] #quita primera fila

head(dataStaHelena,5)

data <- data.frame(dataStaHelena$V1, dataStaHelena$V3, dataCentro$V3) ## siata
data <- data.frame(dataStaHelena$V5, dataStaHelena$V7, dataCentro$V7) ## openaq

data <- tail(data,10)
colnames(data) <- c("date","staHelena","centro")

data$centro <- as.numeric(levels(data$centro))[data$centro]
data$staHelena <- as.numeric(levels(data$staHelena))[data$staHelena]
data$emission <- (data$centro - data$staHelena)
data$total <- data$emission + data$centro


fig <- plot_ly(data, x = ~date, y = ~((centro/total)*100), type = 'bar', name = 'Ref Atmósfera StaHelena')
fig <- fig %>% add_trace(y = ~((emission/total)*100), name = 'Ref Emision Centro')
fig <- fig %>% layout(yaxis = list(title = 'Porcentaje'), barmode = 'stack')

fig

## add ICApm25 colors
set_color <- function(x) {
    ifelse(x >0 & x < 13 , "green",
    ifelse(x > 13 & x <= 25 , "yellow",
    ifelse( x > 25 & x <= 35, "orange",
    ifelse( x > 35 & x <= 55, "red",
    ifelse( x > 55 & x <= 100, "purple",
    ifelse( x > 100, "maroon",
           "grey"))))))
}

data_pm25 <- fromJSON("https://api.openaq.org/v1/measurements?city=Medellin&parameter=pm25")
pm25 <- tibble(
    location = data_pm25$results$location,
    date_utc = ymd_hms(data$results$date$local),
    value = data_pm25$results$value,
    color=set_color((data_pm25$results$value)),
    lat = data_pm25$results$coordinates$latitude,
    lng = data_pm25$results$coordinates$longitude
)

data_locations <- fromJSON("https://api.openaq.org/v1/locations?country[]=CO")
locations <- tibble(
    id = data_locations$results$id,
    city = data_locations$results$city,
    location = data_locations$results$location,
    sourceName = data_locations$results$sourceName,
    lat = data_locations$results$coordinates$latitude,
    lng = data_locations$results$coordinates$longitude
)

leaflet(locations) %>%
    addTiles() %>%
    addCircleMarkers(~lng, ~lat, popup = ~location)

leaflet(pm25) %>%
    addTiles() %>%
    addCircleMarkers(~lng, ~lat, popup = ~location, color = ~color)



