# Cargo las librerías
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lubridate)


# Cargo los datos como data.frame
capitulos <- read_csv("capitulos_rladies.csv")
eventos <- read_csv("eventos_rladies.csv")


# Quick look
glimpse(capitulos)
glimpse(eventos)


# Junto ambas tablas para contar eventos y sumar asistentes
rladies <- inner_join(eventos, capitulos, by="capitulo")

rladies$meses_desde_creacion <- time_length(as.duration(rladies$creacion %--% today()), unit="month")

glimpse(rladies)

# Junto por país y cuento capítulos y cantidades de eventos y asistente promedio por evento
pais_capitulo <-    rladies %>%
                      select(pais, capitulo, miembros, respuesta_asistire, meses_desde_creacion) %>%
                        group_by(pais, capitulo) %>%
                          summarise(qty_eventos = n(),
                                    miembros = max(miembros),
                                    meses_desde_creacion = max(meses_desde_creacion),
                                    asistencia_promedio = mean(respuesta_asistire))
# Creo Flag de capítulos Arg
pais_capitulo <- pais_capitulo %>%
                  mutate(flag_arg = case_when(pais == "AR" ~ capitulo,
                                              TRUE ~ ""))
pais_capitulo <- pais_capitulo %>%
                  mutate(flag_spain = case_when(pais == "ES" ~ capitulo,
                                              TRUE ~ ""))


# Agrupo y ordeno por país:
# 1) Cantidad de capítulos
# 2) Total de Eventos generados
# 3) Total de miembros por país
# 4) Promedio de miembros por capítulo de cada país
# 5) Asistencia promedio a los eventos
pais_capitulo %>%
    group_by(pais) %>%
      summarise(capitulos = n(),
                eventos = sum(qty_eventos),
                total_miembros = sum(miembros),
                miembros_promedio = mean(miembros),
                asistencia_promedio = mean(asistencia_promedio)) %>%
      arrange(desc(asistencia_promedio))
      

# Grafico cantidad de miembros vs cantidad de eventos generados, y muestro asistencia promedio a cada evento
# Agrego los labels de los capítulos de Argentina
pais_capitulo %>%
    ggplot(aes(x=miembros,y=(qty_eventos/meses_desde_creacion),size=asistencia_promedio)) +
    geom_point(color="darkmagenta") +
    geom_label_repel(aes(label=flag_arg), size=2, colour="deepskyblue3") +
    xlab("Número de miembros del capítulo") + ylab("Cantidad de eventos por cada mes desde creación del capítulo")


pais_capitulo %>%
  #filter(miembros < 2000, meses_desde_creacion >= 3) %>%
  ggplot(aes(x=miembros,y=(qty_eventos/meses_desde_creacion),size=asistencia_promedio)) +
  geom_point(color="darkmagenta") +
  geom_label_repel(aes(label=flag_spain), size=2, colour="firebrick2") +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Número de miembros del capítulo") + ylab("Cantidad de eventos por cada mes desde creación del capítulo")


