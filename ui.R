library(shiny)
library(leaflet)
library(shinycssloaders)

## Define UI for dataset viewer application
## checkboxInput("legend", "Mostrar leyenda", TRUE),
ui <- function(request) {
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
      top = 5, right = 5,
      h4("CanAirIO"),
      h3("Citizen network for monitoring air quality"),
      a("Acerca de este proyecto", href = "https://canair.io/"),
      br(),
      a("Panel de datos", href = "http://influxdb.canair.io:8000/"),
      selectInput(
        "ciudad", "Ubicación/Location:",
        c(
          "Global" = "global",
          "Bogotá" = "bogota",
          "Cali" = "cali",
          "Medellín" = "medellin",
          "Lima" = "lima",
          "Bilbao" = "bilbao",
          "Berlín" = "berlin"
        )
      ),
    )
  )
}
