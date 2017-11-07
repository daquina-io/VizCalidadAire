

require(readr)
require(lubridate)
require(rbokeh)
points <- read_csv("https://github.com/daquina-io/VizCalidadAire/raw/master/data/points.csv")

filterPoints <- function(df,col, maxValue, minValue){
 (df[,col] > minValue & df[,col] < maxValue)
}

points_fltr<- points[filterPoints(points, "lat", 6.6, 5.3) & filterPoints(points, "lng", -76, -74),]


## hexbin con mapa de fondo
gmap(lat = 6.25, lng = -75.57, zoom = 13, width = 700, height = 600, api_key = "AIzaSyAPrAJ3958g2fg80Xd8brVpPjNleMExZNs") %>%
  ly_hexbin(as.numeric(points_fltr$lng), as.numeric(points_fltr$lat), as.numeric(points_fltr$pm25), alpha =0.5, xbins = 200)

## hexbin sin mapa
figure(width = 800, height = 450, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_hexbin(as.numeric(points_fltr$lng), as.numeric(points_fltr$lat), as.numeric(points_fltr$pm25))
