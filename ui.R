library(shiny)
library(leaflet)


## Define UI for dataset viewer application
## checkboxInput("legend", "Mostrar leyenda", TRUE),
ui <- function(request) {
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 5, right = 5,
                  titlePanel("Mediciones AQA"),
                  h3("Calidad del aire"),
                  a("Acerca de este proyecto",  href="http://wiki.unloquer.org/personas/brolin/proyectos/agentes_calidad_aire"),
                  sliderInput("range", "Concentración partículas PM2.5 (ug/m3)", 0, 500,value = c(10,200), step = 1 ),
                  
                  bookmarkButton()
                  )
  )
}
