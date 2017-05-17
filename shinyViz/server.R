if(!require(shiny)) install.packages('shiny')
if(!require(dplyr)) install.packages('dplyr')
if(!require(leaflet)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
}

## intervalos fechas
intervalo_fechas <- function(start_end) { (interval(start_end[1],start_end[2])) }

shinyServer(function(input, output) {
  data <- reactive({
    servicios %>% filter(municipio_nombre %in% input$select, fechaServicio %within% intervalo_fechas(input$dates)) %>% group_by(tipo,nombre,municipio_nombre) %>% summarise(cantidad = n()) -> servicios_tipo
    servicios_tipo
  })
  # You can access the values of the widget (as a vector of Dates)
  # with input$dates, e.g.
  output$value1 <- renderPrint({ input$dates })
  # You can access the value of the widget with input$select, e.g.
  output$value2 <- renderPrint({ input$select })
  output$grafico <- renderPlotly({
    p <- ggplot(data(), aes(nombre, cantidad, fill = tipo)) + geom_bar(stat = "identity")
    (gg <- ggplotly(p))

  })
})
