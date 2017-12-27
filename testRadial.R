
library(plotly)
library(reshape2) # for melt
require(readr)
require(lubridate)
require(ggplot2)
devtools::install_github('jbkunst/highcharter')
require(highcharter)
require(stringr)
require(influxdbr)


## ## leer de csv
points <- read_csv("~/Projects/daquina/VizCalidadAire/data/Material_Particulado.csv")
## quita NA
points <- points[complete.cases(points), ]
points$time<- mdy_hms(points$time)
## agrega escala ICA
points$ica <- sapply(points$mean_pm25, function(x){
  if(x <= 15 ) return("green")
  if(x > 16 & x <= 30) return("yellow")
  if(x > 31 & x <= 50) return("red")
  else return("purple")
  })

## Visualizaciones
### Serie de tiempo
plot(points$time, points$mean_pm25)

## plotly

### area
p <- plot_ly(points, r = ~mean_pm25, t = ~time) %>% add_area(color = ~ica)
layout(p, radialaxis = list(ticksuffix = "ug/m3"), orientation = 270)

### scatter
p <- plot_ly(points, r = ~mean_pm25, t = ~time, color = ~ica, alpha = 0.5, type = "scatter")
layout(p, radialaxis = list(ticksuffix = "ug/m3"))

### To Audio meters X42
points <- read_csv("~/Data/aire/AQAdata/volker0004.csv")


### Read data from influxdb
con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

repeat{
  points <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"mean_pm25\", mean(\"lat\") AS \"mean_lat\", mean(\"lng\") AS \"mean_lng\" FROM \"aqa\".\"autogen\".\"volker0001\" WHERE time > now() - 1h GROUP BY time(1h)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))
  points <- as.data.frame(points)
  ## quita NA
  points <- points[complete.cases(points), ]
  ## refine pm25
  ## change range between 0 and 1
  x <- points$mean_pm25
  newMax <- 100
  newMin <- 0
  oldMin <- 0
  oldMax <- 500
  y <- unlist(lapply(x, function(x)(((x-oldMin)*newMax)/oldMax)+newMin))
  print(y)
  ## write file
  write(y, file="pm25.txt", ncolumns = 1)
  ## Sys.sleep() will not work if the CPU usage is very high; as in other critical high priority processes are running (in parallel).
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<5.5){} #dummy while loop
}

rwData <- function(sensor, file.name){
  repeat{
    points <- influx_query(con, db = "aqa", query = paste("SELECT mean(\"pm25\") AS \"mean_pm25\", mean(\"lat\") AS \"mean_lat\", mean(\"lng\") AS \"mean_lng\" FROM \"aqa\".\"autogen\".",sensor," WHERE time > now() - 1h GROUP BY time(1h)"),timestamp_format = c("n", "u", "ms", "s", "m", "h"))
    points <- as.data.frame(points)
    ## quita NA
    points <- points[complete.cases(points), ]
    ## refine pm25
    ## map ranges
    x <- points$mean_pm25
    newMax <- 100
    newMin <- 0
    oldMin <- 0
    oldMax <- 500
    y <- unlist(lapply(x, function(x)(((x-oldMin)*newMax)/oldMax)+newMin))
    print(paste(sensor, y))
    ## write file
    write(y, file=file.name, ncolumns = 1)
    ## run time 
    ## Sys.sleep() will not work if the CPU usage is very high; as in other critical high priority processes are running (in parallel).
    date_time<-Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time))<5.5){}  # dummy while loop
  }
}
rwData("volker0001", "volker0001.txt")
rwData("volkuerC3p", "volkerC3p.txt")


