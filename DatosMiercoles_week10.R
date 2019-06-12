# Cargo las librerías a utilizar
library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)


# Levanto los datos
rating_vinos <- readr::read_csv("vinos.csv")

glimpse(rating_vinos)

# Pasando a factores
rating_vinos$pais <- as.factor(rating_vinos$pais)
rating_vinos$provincia <- as.factor(rating_vinos$provincia)
rating_vinos$region_1 <- as.factor(rating_vinos$region_1)
rating_vinos$variedad <- as.factor(rating_vinos$variedad)


#####################
# Exploración

# Por país, relación precio-puntaje
rating_vinos %>%
  #filter(pais %in% c("Argentina","Chile","Uruguay","Estados Unidos")) %>%
  filter(pais %in% c("Argentina","Chile")) %>%
  filter(precio < 1000) %>%
  ggplot(aes(x=precio, y=puntos, color=pais)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = 'lm', formula = y ~ log(x))

# Por país, boxplot de precios
rating_vinos %>%
  filter(pais %in% c("Argentina","Chile")) %>%
  filter(precio < 1000) %>%
  ggplot(aes(x=pais, y=puntos, color=pais)) +
  geom_boxplot()


# Tipo de Vino por país
freq_variedad <- rating_vinos %>%
                  filter(pais == "Argentina") %>%
                  group_by(variedad) %>%
                  summarise(n = n()) %>%
                  arrange(desc(n))
  
# Flag de Tinto vs Blanco
vino_arg <-   rating_vinos %>%
              filter(pais == "Argentina") %>%
              mutate(tipo = case_when(variedad %in% c("Chardonnay","Torrontés",
                                                      "Sauvignon Blanc","Ensamblaje Blanco",
                                                      "Pinot Grigio","Viognier",
                                                      "Sémillon","Moscato",
                                                      "Pinot Gris","Chardonnay-Viognier",
                                                      "Chenin Blanc-Chardonnay","Chenin Blanc",
                                                      "Chardonnay-Sauvignon","Riesling",
                                                      "Chardonnay-Semillon","Gewürztraminer",
                                                      "Moscatel","Tocai",
                                                      "Trebbiano") ~ "blanco",
                                      variedad %in% c("Rosé","Rosado") ~ "rosado",
                                      variedad == "Champagne Ensamblaje" ~ "espumante",
                                      TRUE ~ "tinto"))
  
# Ploteo relación precio/calidad por tipo de vino
vino_arg %>%
  ggplot(aes(x=precio, y=puntos, color=tipo)) +
  geom_jitter(alpha=0.1, height = 0.5) +
  stat_smooth(method = 'lm', formula = y ~ log(x))
  

# Estimo función puntos = log(precio) para tintos y blancos
red <- vino_arg %>%
  filter(tipo == "tinto")
red_class <- lm(puntos ~ log(precio), data = red)

white <- vino_arg %>%
  filter(tipo == "blanco")
white_class <- lm(puntos ~ log(precio), data = white)

red$puntaje_estimado <- predict(red_class, red)
white$puntaje_estimado <- predict(white_class, white)


# Vuelvo a juntar los datos
vino_arg_estimado <- rbind(red,white)


# Clasifico cada vino por si está por arriba o por debajo de lo esperado
vino_arg_estimado <- vino_arg_estimado %>%
  mutate(clase_esperada = case_when(puntos >= puntaje_estimado ~ "01 Debajo de lo Esperado",
                                    puntos < puntaje_estimado ~ "02 Arriba de los Esperado",
                                    TRUE ~ "Sin Clasificar"))
# Ploteo
vino_arg_estimado %>%
  ggplot(aes(x=precio, y=puntos, color=as.factor(clase_esperada))) +
  geom_point(alpha=0.2, size=2)


# Marco los 10 vinos tintos y blancos q mejor performan por el precio que tienen
blanco_order <- vino_arg_estimado %>%
                        filter(tipo == "blanco") %>%
                          arrange(clase_esperada, desc(puntos))
blanco_order$order_t <- NA
blanco_order$order_b <- 1:length(blanco_order$puntos)



tinto_order <- vino_arg_estimado %>%
                        filter(tipo == "tinto") %>%
                        arrange(clase_esperada, desc(puntos))
tinto_order$order_t <- 1:length(tinto_order$puntos)
tinto_order$order_b <- NA


# Vuelvo a juntar...
vino_arg_order <- rbind(blanco_order, tinto_order)


vino_arg_order <- vino_arg_order %>%
                        mutate(label_b = case_when(order_b < 6 ~ str_c(nombre, vina, sep=" - "),
                                                   TRUE ~ ""),
                               label_t = case_when(order_t < 6 ~ str_c(nombre, vina, sep=" - "),
                                                   TRUE ~ ""))


# Plotting
library(ggrepel)
vino_arg_order %>%
  #filter(n > 10) %>%
  ggplot(aes(x=precio ,y=puntos, color=tipo)) + 
  geom_point(alpha = 0.15) +
  scale_fill_manual(values=c("maroon3", "lightgoldenrod"))+
  #geom_label_repel(aes(label=label_b), size=2.5, colour="limegreen") +
  geom_label_repel(aes(label=label_t), size=2, colour="indianred3") +
  xlab("Precio") + ylab("Puntaje Wine Spectator")






