# Loading libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)


# Importing the data
gm <- read.csv("C:/Users/B037123/Documents/99 - Pedidos Ad Hoc/99 WedData/gapminder.csv",sep=",",header = T)

# Checking data
glimpse(gm)

# Calculating the last year of available data for each country
last_year_country <- gm %>% group_by(pais) %>% summarise(anio = max(anio)) %>% data.frame

# Inner join to keep all variables for each country for last data
gm_last <- inner_join(x = gm, y = last_year_country, by = c("pais","anio"))

top10 <- top_n(gm_last, 10, poblacion)


# Simple plot: PIB x E_0, by country and color by continent
gm_last %>%
    ggplot(aes(x=pib_per_capita, y=esperanza_de_vida, color=continente)) + geom_point(aes(size=poblacion), alpha=0.45) +
    geom_label_repel(aes(label=pais), size = 2, data=top10) +
    xlab("PIB per Cápita") + ylab("Esperanza de Vida al Nacer")




