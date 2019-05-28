# Cargando paquetes necesarios
library(tidyverse)
library(rworldmap)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(countrycode)


# Importando los datos
parlamento <- read.csv("dato_uip.csv", header=T, encoding = "UTF-8")

glimpse(parlamento)

# Elimino las filas donde no hay número de integrantes (no existe la cámara)
parlamento <- parlamento %>% filter(numero_integrantes != "NA")

# Un poco de Discovery
prop.table(table(parlamento$cuota_genero,parlamento$camara),2)
# a nivel cupo femenino y edad mínima de integrantes, las cámaras bajas puede ser comparadas con las únicas

parlamento %>%
  ggplot(aes(x=camara, y=integrante_mas_joven)) +
  geom_boxplot()

# Genero variable de distancia entre el miembro más joven y el mínimo
parlamento <- parlamento %>% mutate(diff_edad = integrante_mas_joven - edad_elegibilidad)


map_genero <- parlamento %>% filter(camara == "baja" | camara == "única", cuota_genero == "Sí") %>%
        select(iso_pais, integrante_mas_joven, porcentaje_mujeres, diff_edad) %>%
        joinCountryData2Map(joinCode = "NAME", nameJoinColumn = "iso_pais")

map_no_genero <- parlamento %>% filter(camara == "baja" | camara == "única", cuota_genero == "No") %>%
  select(iso_pais, integrante_mas_joven, porcentaje_mujeres, diff_edad) %>%
  joinCountryData2Map(joinCode = "NAME", nameJoinColumn = "iso_pais")


mapCountryData(map, nameColumnToPlot = "integrante_mas_joven", 
               catMethod = "quantiles",
               colourPalette = c("#2b83ba","#abdda4","#ffffbf","#fdae61","#d7191c"))


mapCountryData(map_genero, nameColumnToPlot = "porcentaje_mujeres", 
               #numCats = 5,
               catMethod = c(0,10,20,30,50,65),
               #catMethod = "quantiles",
               addLegend=F,
               
               mapTitle = "Porcentaje de Mujeres en Cámaras Bajas y Únicas donde SÍ hay cuota de Género",
               oceanCol = "#f1fbf6",
               colourPalette = c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))

mapParams <-   mapCountryData(map_genero, nameColumnToPlot = "porcentaje_mujeres", 
               #numCats = 5,
               catMethod = c(0,10,20,30,50,65),
               #catMethod = "quantiles",
               addLegend=F,
               
               mapTitle = "Porcentaje de Mujeres en Cámaras Bajas y Únicas donde SÍ hay cuota de Género",
               oceanCol = "#f1fbf6",
               colourPalette = c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))

do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5 ))


mapCountryData(map_no_genero, nameColumnToPlot = "porcentaje_mujeres", 
               #numCats = 5,
               catMethod = c(0,10,20,30,50,65),
               mapTitle = "Porcentaje de Mujeres en Cámaras Bajas y Únicas donde NO hay cuota de Género",
               oceanCol = "#f1fbf6",
               colourPalette = c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))




ggplot(data = map) +
  geom_sf(aes(fill = map$diff_edad)) +
  scale_fill_viridis_c(option = "plasma")
