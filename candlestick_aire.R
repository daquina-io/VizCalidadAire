rm(list=ls())

require(influxdbr)
require(ggplot2)
library(plotly)
library(xts)
library(quantmod)
library(lubridate)
library(tidyverse)

### Read data from influxdb
##con <- influx_connection(scheme = c("http", "https"), host = "aqa.unloquer.org",port = 8086, group = NULL, verbose = FALSE, config_file = "~/.influxdb.cnf")

##  points <- influx_query(con, db = "aqa", query = "SELECT mean(\"pm25\") AS \"mean_pm25\", mean(\"lat\") AS \"mean_lat\", mean(\"lng\") AS \"mean_lng\" FROM \"aqa\".\"autogen\".\"volker0008\" WHERE time > now() - 30d GROUP BY time(1h)",timestamp_format = c("n", "u", "ms", "s", "m", "h"))

## volver points xts siguiendo el blog
## https://openanalytics.wordpress.com/2014/02/27/creating-candlestick-charts-in-r-for-non-financial-data/
##apply.daily(points,function(x) apply(x,2,mean))
##points <- to.daily(points$date) ## TODO probar, puede ahorrar la creación del OHLC

## ## leer de csv
points <- read.csv("mediciones_aire.csv")
points <- points[complete.cases(points), ]
colnames(points) <- c("time","cant" )
points$time <- as.POSIXct(points$time)
## summarize by day
pointsDF <- data.frame(points %>%
                       group_by(Day = as.Date(time))%>%
                       summarise(mean = mean(cant)))

pointsDF <- xts(pointsDF[ ,-1], order.by=pointsDF[ ,1] )
Date <- as.POSIXct(rownames(pointsDF[ ,-1]))
colnames(pointsDF) <- c("cant" )

Open <- points %>%
    group_by(as.Date(time)) %>%
    summarise(cant = first(cant))
pointsDF$Open <- Open$cant

High <- points %>%
    group_by(as.Date(time)) %>%
    summarise(cant = max(cant))
pointsDF$High <- High$cant

Low <- points %>%
    group_by(as.Date(time)) %>%
    summarise(cant = min(cant))
pointsDF$Low <- Low$cant

Close <- points %>%
    group_by(as.Date(time)) %>%
    summarise(cant = last(cant))
pointsDF$Close <- Close$cant

## chg <- ifelse(diff(Open$cant) > 0, "up", "dn")
## chg <- append(chg,"up", after = 0)  ## TODO verificar con DF
## ##chg[1] <-  "up"  ## HACK
## pointsDF$width <- 86400 ## as.numeric(periodicity(Date))
## pointsDF <- merge(pointsDF, chg)  ## ERROR voy aca

chartSeries(pointsDF, theme="black", type='candles', name = "Mediciones calidad del aire Medellín (Sector Montesori)", ylabel="PM2.5", up.col = "orange", dn.col = "green")
