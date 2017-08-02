library(shiny)
library(leaflet)


## Define UI for dataset viewer application
## checkboxInput("legend", "Mostrar leyenda", TRUE),
ui <- function(request) {
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  titlePanel("Contaminación del aire en Medellín"),
                  sliderInput("range", "Concentración partículas PM2.5 (ug/m3)", 0, 500,value = c(80,100), step = 1 ),
                  dateRangeInput("dates", start = "2017-06-01", end = "2018-01-01", label = h3("Rango de fechas")),
                  sliderInput("hours", "Horas",0,24, value=c(6,9), step = 1), 
                  selectInput("wday", "Día de la semana", c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), multiple=TRUE, selectize=TRUE, selected = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
                  bookmarkButton()
                  )
  )
}
