library(shiny)
library(leaflet)

## Define UI for dataset viewer application
ui <- function(request) {
  shinyUI(fluidPage(
    titlePanel("Reporte de servicios por municipio"),
    sidebarLayout(
      sidebarPanel(

        dateRangeInput("dates", start = "2015-01-01", end = "2016-12-31", label = h3("Rango de fechas")),

      ),
      mainPanel(
        leafletOutput("map")
      )
    )
  ))
}
