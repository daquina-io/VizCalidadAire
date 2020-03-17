library(plotly)
library(tidyverse)

Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

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
