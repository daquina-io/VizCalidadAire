library(shiny)
library(leaflet)


## Define UI for dataset viewer application
## checkboxInput("legend", "Mostrar leyenda", TRUE),
ui <- function(request) {
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 5, right = 5,
                  h3("Mediciones Móviles AQA"),
                  a("Acerca de este proyecto",  href="http://wiki.unloquer.org/personas/brolin/proyectos/agentes_calidad_aire"),
                  sliderInput("integer", "Tamaño del rastro", min = 1, max = 10,value = 1 )
                  )
  )
}
