# Levanto las librerías necesarias
library(tidyverse)
library(ggplot2)
library(readr)
library(lubridate)

# Importo los datos
tuberculosis_oms <- read.csv("tuberculosis_oms.csv", encoding="UTF-8")

# Miramos qué hay ahí dentro
glimpse(tuberculosis_oms)

# Reshaping la base para convertir variables fila en columna
# Tipo Diagnóstico: fpp, fpn, ep, recaídas
# Género: m, f
# Edad: 0-14, 15-24,25-34,35-44,45-54,55-64,+65
tuber <- tuberculosis_oms %>%
            gather(codigo, valor, -c(1:4))

tuber <- tuber %>%
            separate(codigo, c("tipo","diagnostico","sexo_edad"))

tuber <- tuber %>%
            separate(sexo_edad, c("sexo","edad"), sep=1)

# Pasando las variables a tipos correctos
tuber$tipo <- as.factor(tuber$tipo)
tuber$diagnostico <- as.factor(tuber$diagnostico)
tuber$sexo <- as.factor(tuber$sexo)
tuber$edad <- as.factor(tuber$edad)

glimpse(tuber)

table(tuber$diagnostico)

# Miremos cuánto dato vacío hay...
library(naniar)
vis_miss(tuber, warn_large_data = F)
# 18.76% de los datos de valor están vacíos...


# Primero intento de ploteo
tuber %>%
  filter(pais == "España" | pais == "Argentina", sexo == "h", edad == "2534") %>%
  group_by(anio) %>%
  ggplot(aes(x = anio, y =valor, color=pais, shape=diagnostico)) +
  geom_line()


# Armo tabla para comparar Arg vs España
esp_vs_arg <- tuber %>% filter(pais == "España" | pais == "Argentina", !is.na(valor))

# Levanto población por pais por año, y hago inner join
# Tomado de: https://github.com/datasets/population
poblacion <- read.csv("population_by_country.csv", header=T, encoding = "UTF-8")
names(poblacion) <- c("pais","iso3","anio","poblacion")

# Agrupo total de casos para todo diagnóstico nuevo, ambos sexos, todas las edades
esp_vs_arg_group <- esp_vs_arg %>% 
                      group_by(iso3, anio) %>% 
                        filter(diagnostico == "fpp") %>%
                        summarise(casos_tuberculosis = sum(valor))


# Joineo todo
esp_vs_arg_3 <- inner_join(esp_vs_arg_group, poblacion, by=c("iso3","anio"))


esp_vs_arg_3 <- esp_vs_arg_3 %>%
                  mutate(tasa_tuberculosis = casos_tuberculosis / poblacion)

# Ploteo
esp_vs_arg_3 %>%
  ggplot(aes(x=anio, y=(tasa_tuberculosis*100000), color=as.factor(iso3))) +
  geom_line() +
  scale_x_continuous(breaks=seq(1995,2014,2)) +
  scale_color_manual(values = c("ARG"="blue","ESP"="red")) +
  geom_text(aes(label=round(tasa_tuberculosis*100000,0)), nudge_y=1) + 
  xlab("Año") + ylab("Casos de Tuberculosis por cada 100k habitantes
                     con diagnóstico 'frotis pulmonar positivo'") +
  labs(color = "País") + 
  theme(axis.text.x = element_text(angle = 45))

# Transformo en gif
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)

p <- esp_vs_arg_3 %>%
        ggplot(aes(x=anio, y=round(tasa_tuberculosis*100000,0), color=as.factor(iso3))) +
        geom_line() +
        scale_x_continuous(breaks=seq(1995,2014,2)) +
        scale_color_manual(values = c("ARG"="blue","ESP"="red")) +
        geom_text(aes(label=round(tasa_tuberculosis*100000,0)), nudge_y=1) + 
        xlab("Año") + ylab("Casos de Tuberculosis por cada 100k habitantes
                           con diagnóstico 'frotis pulmonar positivo'") +
        labs(color = "País") + 
        theme(axis.text.x = element_text(angle = 45)) +
        transition_reveal(anio) 

p2 <- animate(p, nframes=40, fps = 2)
anim_save("tasa_tuberculosis_Arg_vs_Spain.gif", p2)



############################################################
# Aumentó la tasa de incidencia de tuberculosis worldwide?




