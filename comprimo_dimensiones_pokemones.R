# Cargo las librerías necesarias para el análisis
library(tidyverse)
library(ggplot2)
library(Rtsne)

# Imnporto los datos
poke <- read_csv("Pokemon_simple_es.csv")

# Miramos qué hay dentro
glimpse(poke)

# Paso a factores los tipos de Pokemon
poke$tipo_1 <- as.factor(poke$tipo_1)
poke$tipo_2 <- as.factor(poke$tipo_2)


# Convierto los nulos de "nivel_evolución" en 0's
# Paso a binario el flag de Legendario
poke <- poke %>% mutate(es_legendario = case_when(es_legendario == "FALSO" ~ 0,
                                  TRUE ~ 1))

poke <- poke %>% mutate(nivel_evolución = case_when(is.na(nivel_evolución) ~ 0,
                                  TRUE ~ nivel_evolución))


# Plot exploratorio
poke %>%
  ggplot(aes(x=ataque, y=defensa, color=as.factor(es_legendario))) + 
  geom_point()

poke %>%
  ggplot(aes(x=tipo_1, y=ataque)) + 
  geom_boxplot()

poke %>%
  ggplot(aes(x=as.factor(es_legendario), y=total)) + 
  geom_boxplot()

# Legendario por Tipo 1
prop.table(table(poke$tipo_1, poke$es_legendario),2)

# Resumen por Tipo 1
poke %>%
  filter(es_legendario==0) %>%
  group_by(tipo_1) %>%
    summarise(hp = round(mean(puntos_vida),0),
              attack = round(mean(ataque),0),
              defense = round(mean(defensa),0),
              velocity = round(mean(velocidad),0))



#########################
# PCA vs t-SNE

# Seteo seed para mantener los resultados
set.seed(31416)

# Genero PCA de variables 
var_compresion <- poke %>% select("puntos_vida","ataque","defensa",
                                  "velocidad_ataque","velocidad_defensa","velocidad")

poke_pca <- prcomp(var_compresion, scale. = T)
summary(poke_pca)
plot(poke_pca)
poke_pca$rotation

base_pca <- cbind(poke[,1:5], poke_pca$x[,1:2], poke[,13], poke[,14])
names(base_pca) <- c("ID","Name","tipo_1","tipo_2","total","pca_1","pca_2","es_legendario", "nivel_evol")

base_pca %>%
  ggplot(aes(x=pca_1, y =pca_2, color=as.factor(es_legendario))) +
  geom_point()

base_pca %>%
  ggplot(aes(x=pca_1, y =pca_2, color=as.factor(nivel_evol))) +
  geom_point()

base_pca %>%
  ggplot(aes(x=pca_1, y =pca_2, color=total)) +
  geom_point()


# Genero t-SNE de variables
poke_tsne <- Rtsne(var_compresion,dims=2, perplexity=45, verbose=TRUE, max_iter = 1000, check_duplicates = FALSE)
summary(poke_tsne)


base_tsne <- cbind(poke[,1:5], poke_tsne$Y[,1], poke_tsne$Y[,2], poke[,13], poke[,14])
names(base_tsne) <- c("ID","Name","tipo_1","tipo_2","total","tsne_1","tsne_2","es_legendario", "nivel_evol")
base_tsne %>%
  ggplot(aes(x=tsne_1, y =tsne_2, color=as.factor(es_legendario))) +
  geom_point()

base_tsne %>%
  ggplot(aes(x=tsne_1, y =tsne_2, color=total)) +
  geom_point()

base_tsne %>%
  ggplot(aes(x=tsne_1, y =tsne_2, color=as.factor(tipo_1))) +
  geom_point()

base_tsne %>%
  filter(nivel_evol != 0) %>%
  ggplot(aes(x=tsne_1, y =tsne_2, color=as.factor(nivel_evol))) +
  geom_point()


