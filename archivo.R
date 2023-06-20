#Función que instala los paquetes dados
packages <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs,require, character.only = TRUE))
    need <- libs[req==FALSE]
    if(length(need)>0){
        install.packages(need)
        lapply(need, require, character.only = TRUE)
    }
}
packages("readr", "ggplot2", "dplyr")

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
  duration <- duration /1000
  minutos <- duration %/% 60
  segundos_restantes <- duration %% 60
  resultado <- sprintf("%d.%02d", as.integer(minutos), as.integer(segundos_restantes))
  return(resultado)
}
#Usando la función
spotify$duration <- sapply(spotify$duration, convert)
spotify$duration <- as.numeric(spotify$duration)

#!duración tiene demasiado rango (posibles atípicos?)
#Eliminar aquellas donde la duracion sea mayor a 20 min - baja de 156608 a 156353
spotify <- subset(spotify, duration <= 20)
#Eliminar aquellas donde la duracion sea menor a 1 min - baja de 155976 a 154524
spotify <- subset(spotify, duration >= 1)

#!explicit debería ser factor (1 si, 0 no)
#CAMBIANDO VALORES DE LA COLUMNA "MODE"
spotify$mode <- ifelse(spotify$mode == 0, "Menor", "Mayor")
#!mode debería ser factor (1 mayor, 0 menor)

#!Eliminar id y release_date (ya tenemos el año)
spotify <- subset(spotify, select = id)

#!Reordenando columnas
spotify <- spotify[, c("artists", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo", "key", "mode" )]


#!Duplicados:
#Eliminar donde se repite nombre de artista y cancion - baja de 169909 a 156608
spotify <- subset(spotify, !duplicated(paste(artists, name)))

#Corregir el nombre del artista para quitar [', ', y ']
spotify$artists <- gsub("\\['|'|'\\]", "", spotify$artists)

#Corregir el nombre del artista para quitar [", ", y "]
spotify$artists <- gsub("\\[\"|\"\\]", "", spotify$artists)

#Pasar todo a minúsculas para facilitar filtrado
spotify$artists <- sapply(spotify$artists, tolower)
spotify$name <- sapply(spotify$name, tolower)

#*VUELVO A APLICAR EL FILTRO PARA ELIMINAR DUPLICADOS, baja de 156353 a 155976

#!Redondeos
#redondear tempo (bpm)
spotify$tempo <- round(spotify$tempo, 0)

#redondear valence
spotify$valence <- round(spotify$valence, 2)

#redondear dance
spotify$dance <- round(spotify$dance, 2)

#redondear acoust
spotify$acoust <- round(spotify$acoust, 2)

#redondear liveness
spotify$liveness <- round(spotify$liveness, 2)

#redondear energy
spotify$energy <- round(spotify$energy, 2)

#redondear speechness
spotify$speech <- round(spotify$speech, 2)

#cambio de rango en la variable loudness, para pasar de -60/0 a 40/100, y redondear
spotify$loudness <- round(spotify$loudness + 100, 0)

#no mostrar instrumentalness como formatio cientifico
spotify$instr <- format(spotify$instr, scientific = FALSE)
spotify$instr <- as.numeric(spotify$instr)
spotify$instr <- round(spotify$instr, 2)

#spotify$year <- ifelse(spotify$year >= 1920 & spotify$year <= 1929, "1920s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1930 & spotify$year <= 1939, "1930s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1940 & spotify$year <= 1949, "1940s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1950 & spotify$year <= 1959, "1950s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1960 & spotify$year <= 1969, "1960s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1970 & spotify$year <= 1979, "1970s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1980 & spotify$year <= 1989, "1980s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 1990 & spotify$year <= 1999, "1990s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 2000 & spotify$year <= 2009, "2000s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 2010 & spotify$year <= 2019, "2010s", spotify$year)
#spotify$year <- ifelse(spotify$year >= 2020 & spotify$year <= 2029, "2020s", spotify$year)

#spotify %>% ggplot(aes(x = year, y = pop)) + geom_point()

