################ TRABAJO FINAL ################
######### GRUPO 1 #########
##### 03/06/2024 #####



### Instalar paquetes necesarios para la realización de este proyecto

#install.packages("readr")
#install.packages("tweetCoin")
#install.packages("netCoin")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("textdata")
#install.packages("syuzhet")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("textplot")
#install.packages("scale")

## Cargar librerias necesarias

library(readr)
library(tweetCoin)
library(netCoin)
library(tidyverse)
library(ggplot2)
library(textdata)
library(tidytext)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(textplot)
library(stopwords)
library(gganimate)
library(dplyr)

# Datos utilizados para el trabajo
covid_twt <- read_csv("covid19-tweets-early-late-april_lang_es_country_ES.csv")


# Leer el archivo CSV que contiene las palabras a excluir
palabras_filtro <- read_csv("palabras_filtro.csv", 
                            locale = locale(encoding = "Latin1"))
excluir <- paste(palabras_filtro$paises, collapse = "|")


# Filtro para quitar variables irrelevantes
covid_filtrado <- covid_twt %>% 
  select(c(screen_name, text, created_at, followers_count, place_full_name)) %>% 
  rename(author = screen_name,
         text = text,
         date = created_at,
         followers = followers_count,
         location = place_full_name) %>% 
  filter(!grepl(excluir, text, ignore.case = TRUE)) %>% 
  mutate(text = gsub("\n", " ", text)) %>% 
  distinct(text, .keep_all = TRUE)


## Redes de conspiraciones
## vacuna
vacuna <- covid_filtrado %>% 
  filter(grepl("vacuna", text, ignore.case = TRUE)) 

c_vacuna <- cotweet(vacuna, "text", min = 2)
c2_vacuna <- netCoin(c_vacuna, size = "degree", distance = 11.18,
                     repulsion = 20.8, zoom = .7, color = "community", 
                     shape = "type", main= "Grafico Network")
plot(c2_vacuna)

## mascarilla
mascarilla <- covid_filtrado %>% 
  filter(grepl("mascarilla", text, ignore.case = TRUE)) 

c_mascarilla <- cotweet(mascarilla, "text", min = 5)
c2_mascarilla <- netCoin(c_mascarilla, size = "degree", distance = 85.89,
              repulsion = 60.80, zoom = .7, color = "community", 
              shape = "type", main= "Grafico Network")
plot(c2_mascarilla)


## confinamiento
confinamiento <- covid_filtrado %>% 
  filter(grepl("confinamiento", text, ignore.case = TRUE)) 

c_confinamiento <- cotweet(confinamiento, "text", min = 5)
c2_confinamiento <- netCoin(c_confinamiento, size = "degree", distance = 45.29,
                         repulsion = 19.04, zoom = .7, color = "community", 
                         shape = "type", main= "Grafico Network")
plot(c2_confinamiento)


##Análisis de sentimientos
stopwords_es <- stopwords("es")
stopwords_adicionales <- read.csv("stopwords_adicionales.csv", stringsAsFactors = F)$alfabeto
stopwords_final <- c(stopwords_es, stopwords_adicionales)

# Limpiar el texto
vacuna_clean <- vacuna %>%
  mutate(text = str_to_lower(text)) %>%
  mutate(text = str_replace_all(text, "(#\\w+|@\\w+|https\\S+)\\b", "")) %>% 
  mutate(text = str_replace_all(text, "[[:punct:]]", "")) %>%
  mutate(text = str_replace_all(text, "[[:digit:]]", "")) 

# Tokenizar el texto 
vacuna_tokens <- vacuna_clean %>%
  unnest_tokens(word, text)

#filtrar
vacuna_tokens_filtered <- vacuna_tokens %>%
  filter(!word %in% stopwords_final)

#Convertir el token a vector
vacuna_tokens_vector <- c(vacuna_tokens_filtered$word)

# Obtener la polaridad 
polaridad <-  get_nrc_sentiment(vacuna_tokens_vector, language = "spanish")


# Gráfico de barras
barplot(
  colSums(prop.table(polaridad[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Análisis de sentimientos",
  xlab="emociones", ylab = NULL, ylim = c(0, 0.2))



##nube
# Crear el vector de emociones
nube_emociones_vector <- c(
  paste(vacuna_tokens_vector[polaridad$fear > 0], collapse = " "),
  paste(vacuna_tokens_vector[polaridad$sadness > 0], collapse = " "),
  paste(vacuna_tokens_vector[polaridad$trust > 0], collapse = " "),
  paste(vacuna_tokens_vector[polaridad$anticipation > 0], collapse = " ")
)

# Convertir la codificación
nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

# Crear el corpus
nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

# Función para eliminar hashtags
removeHashtags <- content_transformer(function(x, pattern) gsub(pattern, "", x))
nube_corpus <- tm_map(nube_corpus, removeHashtags, "#\\S+")

# Limpiar el texto
nube_corpus <- tm_map(nube_corpus, content_transformer(tolower))
nube_corpus <- tm_map(nube_corpus, removePunctuation)
nube_corpus <- tm_map(nube_corpus, removeNumbers)
nube_corpus <- tm_map(nube_corpus, removeWords, stopwords_final)
nube_corpus <- tm_map(nube_corpus, stripWhitespace)

# Crear la matriz de términos-documentos
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
colnames(nube_tdm) <- c('miedo', 'tristeza', 'confianza', 'anticipación')

# Obtener frecuencias de las palabras
frecuencia_palabras <- rowSums(nube_tdm)
frecuencia_palabras <- sort(frecuencia_palabras, decreasing = TRUE)
words <- names(frecuencia_palabras)
freq <- as.vector(frecuencia_palabras)

# Asignar colores a las emociones
colors <- rep(NA, length(words))
colors[nube_tdm[, "miedo"] > 0] <- "green"
colors[nube_tdm[, "tristeza"] > 0] <- "red"
colors[nube_tdm[, "confianza"] > 0] <- "orange"
colors[nube_tdm[, "anticipación"] > 0] <- "blue"

# Generar nube de comparación
set.seed(600) # puede ser cualquier número
wordcloud(words = words, freq = freq, min.freq = 1, 
          colors = colors, random.order = FALSE, 
          scale = c(2.5, 0.75), max.words = 200)

# Agregar leyenda
legend("topright", legend = c('miedo', 'tristeza', 'confianza', 'anticipación'), 
       fill = c("green", "red", "orange", "blue"), title = "Emociones",
       border = "black",
       bty = "o",
       cex = 0.8, x = 1, y = 0.8)



## Serie temporal

fecha <- as.Date(vacuna_tokens_filtered$date)
tendencia_df <- data.frame(fecha, polaridad)
tendencia <- tendencia_df %>% 
  select(fecha, fear, sadness,trust, anticipation)

# Agrupar los datos por fecha y calcular la media de "fear"
data_agg <- tendencia %>%
  group_by(fecha) %>%
  summarise(mean_fear = mean(fear))

# Identificar máximos relativos
find_local_maxima <- function(x) {
  return((x > lag(x, default = first(x))) & (x > lead(x, default = last(x))))
}

data_agg <- data_agg %>%
  mutate(is_maxima = find_local_maxima(mean_fear))

# Filtrar los puntos que son máximos relativos
maxima_points <- data_agg %>%
  filter(is_maxima)

# Crear el gráfico de líneas con anotaciones para los máximos relativos
ggplot(data_agg, aes(x = fecha, y = mean_fear)) +
  geom_line() +
  geom_point(data = maxima_points, aes(x = fecha, y = mean_fear), color = "red", size = 3) +
  geom_text(data = maxima_points, aes(x = fecha, y = mean_fear, label = fecha), vjust = -1, hjust = 1, color = "blue") +
  labs(title = "Media de Fear a lo largo del tiempo con Máximos Relativos",
       x = "Fecha",
       y = "Media de Fear") +
  theme_minimal() 


# Gráfico de barras de la presencia del miedo en los tweets a lo largo del tiempo

ggplot(tendencia, aes(fecha, fill=fear) ) +
  labs(title = "Presencia de miedo en tweets")+ylab("Grado de miedo") +
  theme(plot.title = element_text(size = rel(2), colour = "Black"))+
  geom_bar(position="dodge")











