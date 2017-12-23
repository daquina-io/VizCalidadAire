
library(plotly)
library(reshape2) # for melt
require(readr)
require(lubridate)
require(ggplot2)
devtools::install_github('jbkunst/highcharter')
require(highcharter)
require(stringr)


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

## Highcharter EXPERIMENTAL---------------------------------------------------------

dsmax <- df %>% 
  mutate(color = colorize_vector(mean_temperaturec, "A"),
         y = max_temperaturec - min_temperaturec) %>% 
  select(x = tmstmp,
         y,
         name = date,
         color,
         mean = mean_temperaturec,
         max = max_temperaturec,
         min = min_temperaturec) %>% 
  list.parse3()


## Some tooltips to make it a little *intercative*
df <- read_csv("http://bl.ocks.org/bricedev/raw/458a01917183d98dff3c/sf.csv")

df[1:4, 1:4]
names(df) <- names(df) %>% 
  str_to_lower() %>% 
  str_replace("\\s+", "_")

df <- df %>% 
  mutate(id = seq(nrow(df)),
         date2 = as.Date(ymd(date)),
         tmstmp = datetime_to_timestamp(date2),
         month = month(ymd(date)))

dsmax <- df %>%
  select(x = tmstmp,
         y = max_temperaturec) %>% 
  list.parse3()
 
dsmin <- df %>% 
  select(x = tmstmp, y = min_temperaturec) %>% 
  list.parse3()

x <- c("Min", "Mean", "Max")
y <- sprintf("{point.%s}", tolower(x))
tltip <- tooltip_table(x, y)

hc <- highchart() %>% 
  hc_chart(
    type = "column",
    polar = TRUE
  ) %>%
  hc_plotOptions(
    series = list(
      stacking = "normal",
      showInLegend = FALSE
    )
  ) %>% 
  hc_xAxis(
    gridLineWidth = 0.5,
    type = "datetime",
    tickInterval = 30 * 24 * 3600 * 1000,
    labels = list(format = "{value: %b}")
  ) %>% 
  hc_yAxis(
    max = 30,
    min = -10,
    labels = list(format = "{value} C"),
    showFirstLabel = FALSE
    ) %>% 
  hc_add_series(
    data = dsmax
  ) %>% 
  hc_add_series(
    data = dsmin,
    color = "transparent",
    enableMouseTracking = FALSE
  ) %>% 
  hc_add_theme(
    hc_theme_smpl()
  ) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")),
    pointFormat = tltip
  )

hc


