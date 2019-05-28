# Cargo los paquetes necesarios para trabajar
library(readr)
library(tidyverse)
library(wesanderson)


# Levanto los datos directo del repossitorio
tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")

cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")


# Miro qué hay dentro de los datasets
glimpse(tiempo_pantalla)

# Minutos por episodio
tiempo_pantalla <- tiempo_pantalla %>%
                    mutate(minutos_promedio = minutos_pantalla / episodios)

# Tiempos promedio de los 10 personajes con más apariciones
tiempo_pantalla %>%
  arrange(desc(minutos_pantalla)) %>%
    top_n(10, minutos_pantalla) %>%
      arrange(desc(minutos_pantalla)) %>%
      ggplot(aes(x= nombre, y=minutos_pantalla, color=nombre)) + geom_point()


# Tiempos promedio de los 10 personajes con más apariciones
tiempo_pantalla$nombre <- factor(tiempo_pantalla$nombre, levels = tiempo_pantalla$nombre[order(tiempo_pantalla$minutos_promedio)])

tiempo_pantalla %>%
  arrange(desc(minutos_promedio)) %>%
  top_n(10, minutos_promedio) %>%
  ggplot(aes(x= nombre, y=minutos_promedio, color=nombre)) + 
    geom_point(size=4, shape=8) +
    geom_text(aes(label=nombre),hjust=0.5, vjust=1.2) +
    #theme(legend.position = "none",  axis.text.x = element_text(size=rel(1.5),angle = 45, hjust = 1)) + xlab("Personaje") + ylab("Minutos por Episodio")
    theme(legend.position = "none",  axis.text.x = element_blank()) + xlab("Personaje") + ylab("Minutos por Episodio")



####################################
# Cuál es el libro más sangriento?
glimpse(personajes_libros)

# Calculo personajes que aparecen en cada libro
personajes_x_libro <- personajes_libros %>%
  select(juego_de_tronos,choque_de_reyes,tormenta_de_espadas,festin_de_cuervos,danza_de_dragones) %>%
  colSums() %>%
  data.frame

# Renombro Columna
names(personajes_x_libro) <- c("Personajes por Libro")
# Renombro Libros
personajes_x_libro$Libro <- c("01 Juego de Tronos",
                              "02 Choque de Reyes",
                              "03 Tormenta de Espadas",
                              "04 Festín de Cuervos",
                              "05 Danza de Dragones")

# Calculo muertes por libro
muertes_x_libro <- 
  personajes_libros %>%
  filter(!is.na(libro_muerte)) %>%
    mutate(Libro = case_when( libro_muerte == 1 ~ "01 Juego de Tronos",
                              libro_muerte == 2 ~ "02 Choque de Reyes",
                              libro_muerte == 3 ~ "03 Tormenta de Espadas",
                              libro_muerte == 4 ~ "04 Festín de Cuervos",
                              libro_muerte == 5 ~ "05 Danza de Dragones",
                              TRUE ~ "none")) %>%
  group_by(Libro) %>%
    summarise(muertes_x_libro = n())


# Junto ambos data sets por libro
death_book <- left_join(personajes_x_libro, muertes_x_libro, by="Libro")

# % de muertes por libro
death_book$perc_muertes <- death_book$muertes_x_libro/death_book$`Personajes por Libro`

# Plotting
death_book %>%
  ggplot(aes(x=Libro, y=round(perc_muertes*100,2), fill=Libro)) + 
  geom_bar(stat="identity") +
  #scale_color_manual(values = wes_palette(n=5, name="Darjeeling1")) +
  geom_text(aes(y=round(perc_muertes*100,2), label = round(perc_muertes*100,2)), size=4.5,vjust=1.6) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",  axis.text.x = element_text(size=rel(1),angle = 45, hjust = 1)) + 
  xlab("") + ylab("Porcentaje Muertes") + ggtitle("Porcentaje de Personajes Muertos por Libro (sobre total de apariciones)")



