library(dygraphs)
library(readr)
library(tidyverse)
library(xts)
library(plotly)
library(ggthemes)
library(readr)
library(flexdashboard)
library(viridis)
library(hrbrthemes)
######## Mapa de calor
choose.files()

Nuevos_casos_departamentos <- read_csv("data/Nuevos_casos_departamentos.csv", 
                                       col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                                        `Nuevos casos` = col_integer()))
diario <- ggplot(Nuevos_casos_departamentos, aes(Fecha, Departamento, fill=`Nuevos casos`)) + 
  geom_tile() + 
  scale_fill_gradient(low = "#ffffff", high = "#1162ac")+
  labs(title = 'Nuevos casos de coronavirus por departamento y fecha',
       x= 'Fechas', y ='') +
  theme_calc()+
  theme(legend.position = 'top')


mapa_calor <- ggplotly(diario)%>% config ( displayModeBar  =  F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% 
  layout(yaxis=list(fixedrange=TRUE)) 
mapa_calor

#### Nuevos casos a nivel nacional 

Nuevos_casos_nacional <- read_csv("~/Datos COVID-19 Bolivia/data/Nuevos_casos_nacional.csv", 
                                  col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                                   `Nuevos casos` = col_integer()))
nuevos_casos <- ggplot(data = Nuevos_casos_nacional) +
  geom_bar(aes(x = Fecha, y = `Nuevos casos`), stat = 'identity', fill = '#000F8E')+
  labs(title = "Nuevos casos por fecha a nivel nacional") +
  scale_y_continuous(breaks = seq(0, 400, by = 50))+
  theme_minimal()
nuevos_casos <- ggplotly(nuevos_casos) %>% config ( displayModeBar  =  F ) 
nuevos_casos
############## Casos acumulados 

Confirmados <- read_csv("~/Datos COVID-19 Bolivia/data/Confirmados.csv", 
                        col_types = cols(`Casos confirmados` = col_integer(), 
                                         Fecha = col_date(format = "%d/%m/%Y")))

Fallecidos <- read_csv("~/Datos COVID-19 Bolivia/data/Fallecidos.csv", 
                       col_types = cols(Fallecidos = col_integer(), 
                                        Fecha = col_date(format = "%d/%m/%Y")))

Recuperados <- read_csv("~/Datos COVID-19 Bolivia/data/Recuperados.csv", 
                        col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                         Recuperados = col_integer()))

Confirmados_xts <- xts(Confirmados$`Casos confirmados`, order.by = Confirmados$Fecha, frequency = 1)

Fallecidos_xts <- xts(Fallecidos$Fallecidos, order.by = Fallecidos$Fecha, frequency = 1)
Recuperados_xts <- xts(Recuperados$Recuperados, order.by = Recuperados$Fecha, frequency = 1)

dinamico <- cbind(Confirmados_xts, Fallecidos_xts, Recuperados_xts)

grafico <- dygraph(dinamico, ylab = 'Cantidad',
                   main = 'Total de casos confirmados, recuperados y fallecidos') %>% 
  dySeries('Confirmados_xts', label = 'Casos') %>% 
  dySeries('Fallecidos_xts', label = 'Fallecidos') %>% 
  dySeries('Recuperados_xts', label = 'Recuperados') %>% 
  dyOptions(colors = c('Blue', 'Black', 'Green')) %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>% 
  dyRangeSelector(height = 20)
grafico


Conf <- read_csv("C:\\Users\\Asus\\Documents\\Datos COVID-19 Bolivia\\data\\Confirmados.csv", 
                 col_types = cols(`Casos confirmados` = col_integer(), 
                                  Fecha = col_date(format = "%d/%m/%Y")))
valor <- Conf[67,2]
choose.files()
