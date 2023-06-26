#Función que instala los paquetes dados
packages <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) {
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}

packages(c("GGally", "readr", "ggplot2", "dplyr",
          "tidyverse", "tibble", "reshape2"))

#Carga de .csv en variable spotify
spotify <- read.csv("data.csv")

#Viendo los datos
head(spotify)
summary(spotify)

#!Cambiar nombre de algunas variables
spotify <- spotify %>% rename(instr = instrumentalness, 
                              duration = duration_ms, pop = popularity, 
                              dance = danceability, speech = speechiness, acoust = acousticness)

#!duration está en ms
#Función para pasar de milisegundos a minutos
convert <- function(duration) {
  duration <- duration / 1000
  minutos <- duration %/% 60
  segundos_restantes <- duration %% 60
  resultado <- sprintf("%d.%02d", as.integer(minutos), as.integer(segundos_restantes))
  return(resultado)
}
#Usando la función
spotify$duration <- sapply(spotify$duration, convert)
spotify$duration <- as.numeric(spotify$duration)

#!duración tiene demasiado rango (posibles atípicos?)
#*Analsis de atípicos:
summary(spotify$duration)
#Grafico para visualizarlo
ggplot(spotify, aes(x = duration, y = duration)) +
  geom_boxplot()


#Extrayendo átipicos del gráfico
atipico_min <- min(boxplot.stats(spotify$duration)$out)
atipicos <- boxplot.stats(spotify$duration)$out

#Creando una nueva columna con información sobre atípico (out = outliner)
spotify$out <- ifelse(spotify$duration > atipico_min, "yes", "no")
spotify$out <- as.factor(spotify$out)

ggplot(spotify, aes(x = out, y = pop, color = out)) +
  geom_boxplot() +
  labs(x = "¿Atípico?", y = "Popularidad") +
  stat_summary(fun = mean, color = "black", shape = 10)
ggplot(spotify, aes(x = pop, fill = out)) +
  geom_histogram() +
  labs(x = "Popularidad", fill = "Atípico")


#!explicit a factor (1 si, 0 no)
spotify$explicit <- ifelse(spotify$explicit == 0, "No", "Yes")
spotify$explicit <- as.factor(spotify$explicit)
#!mode a factor (1 mayor, 0 menor)
spotify$mode <- ifelse(spotify$mode == 0, "Menor", "Mayor")
spotify$mode <- as.factor(spotify$mode)

#!Eliminar id y release_date (ya tenemos el año)
spotify <- subset(spotify, select = -c(id, release_date))

#!Reordenando columnas
col_order <- c("artists", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo", "key", "mode")
spotify <- spotify[, col_order]

#!Duplicados:
#Corregir el nombre del artista para quitar [', ', y ']
spotify$artists <- gsub("\\['|'|'\\]", "", spotify$artists)

#Corregir el nombre del artista para quitar [", ", y "]
spotify$artists <- gsub("\\[\"|\"\\]", "", spotify$artists)

#Pasar todo a minúsculas para facilitar filtrado
spotify$artists <- sapply(spotify$artists, tolower)
spotify$name <- sapply(spotify$name, tolower)

#Eliminar donde se repite nombre de artista y cancion
#Guardando cantidad de observaciones
col_og <- nrow(spotify)
spotify <- subset(spotify, !duplicated(paste(artists, name)))
#Restando cantidad de observaciones viejas con cantidad de observaciones nuevas
col_og - nrow(spotify)
#Quedan eliminadas 13304 observaciones

#!Redondeos
a_redondear <- c("valence", "dance", "acoust", "liveness", "energy", "speech")
for (x in a_redondear) {
  if (x == "tempo") {
    y = 0
  } else { y = 2
  }
  spotify[[x]] <- round(as.numeric(spotify[[x]]), y)
}


#cambio de rango en la variable loudness, para pasar de -60/0 a 40/100, y redondear
spotify$loudness <- round(spotify$loudness + 100, 0)

#no mostrar instrumentalness como formatio cientifico
spotify$instr <- format(spotify$instr, scientific = FALSE)
spotify$instr <- as.numeric(spotify$instr)
spotify$instr <- round(spotify$instr, 2)

#! Creando columnas con décadas
spotify$decada <- NA
spotify$decada <- floor(spotify$year / 10) * 10
spotify$decada <- as.factor(spotify$decada)