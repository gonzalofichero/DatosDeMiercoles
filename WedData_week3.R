# Loading libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(ggparliament)


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


# Parliament Plots
gm_last$votes_per_pop <-  round(sum(gm_last$poblacion),0)
gm_last$votes_per_country <- (gm_last$poblacion / gm_last$votes_per_pop)

# Creating new Global areas
gm_last <- gm_last %>%
              mutate(continente = case_when(pais == "Japón" ~ "Japón",
                                            pais == "China" ~ "China",
                                            pais == "India" ~ "India",
                                            pais == c("Argentina","Chile","Uruguay","Brasil","Paraguay","Bolivia","Perú","Ecuador","Colombia","Venezuela","Panamá","Cuba","México","Costa Rica","Guatemala","Honduras","Nicaragüa","Belice") ~ "LatinoAmérica",
                                            pais == c("",) ~ "Europa Occidental",
                                            pais == c("",) ~ "Europa Oriental",
                                            
                                            
                                            TRUE ~ continente
                                            ))

# By Population
world_parliament <- gm_last %>%
                        group_by(continente) %>%
                        summarise(seats = round(sum(votes_per_country) * 250),0) %>%
                        data.frame()
  
parliament <- parliament_data(election_data = world_parliament,
                              type = "semicircle",
                              parl_rows = 10,
                              party_seats = world_parliament$seats)

# Plotting Parliament seats by Continent
w_rep <-ggplot(parliament, aes(x, y, colour = continente)) +
  geom_parliament_seats(size = 4) + 
  #geom_highlight_government(continente == "Asia", colour = "pink", size = 4) + 
  draw_majoritythreshold(n = 126, 
                         label = TRUE, 
                         linesize = 0.5,
                         type = 'semicircle') + 
  theme_ggparliament() +
  theme(legend.position = 'bottom') + 
  labs(colour = NULL,
       title = "Global Parliament",
       subtitle = "As population in 2007")# +
  #scale_colour_manual(values = parliament$continente, 
  #                    limits = parliament$continente) 

w_rep
