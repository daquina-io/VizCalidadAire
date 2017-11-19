library(shiny)
library(leaflet)


## Define UI for dataset viewer application
## checkboxInput("legend", "Mostrar leyenda", TRUE),
ui <- function(request) {
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 5, right = 5,
                  titlePanel("Mediciones móviles"),
                  h3("Calidad del aire"),
                  a("Acerca de este proyecto",  href="http://wiki.unloquer.org/personas/brolin/proyectos/agentes_calidad_aire"),
                  sliderInput("range", "Concentración partículas PM2.5 (ug/m3)", 0, 500,value = c(10,55), step = 1 ),
                  dateRangeInput("dates", start = "2017-11-01", end = "2018-01-01", label = h3("Rango de fechas")),
                  sliderInput("hours", "Horas",0,24, value=c(12,20), step = 1), 
                  selectInput("wday", "Día de la semana", c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), multiple=TRUE, selectize=TRUE, selected = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
                  bookmarkButton()
                  )
  )
}
