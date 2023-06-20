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

#CARGA DE CSV EN VARIABLE SPOTIFY
spotify <- read.csv("data.csv")

#ELIMINAR ALGUNAS VARIABLES
spotify <- subset(spotify, select = -c(id, release_date))

#CAMBIAR NOMBRES DE ALGUNAS VARIABLES
spotify <- spotify %>% rename(instr = instrumentalness, 
                              duration = duration_ms, pop = popularity, 
                              dance = danceability, speech = speechiness, acoust = acousticness)

#FUNCION PARA PASAR DE MILISEGUNDOS A SEGUNDOS
convert <- function(duration) { 
  duration <- duration /1000
  minutos <- duration %/% 60
  segundos_restantes <- duration %% 60
  resultado <- sprintf("%d.%02d", as.integer(minutos), as.integer(segundos_restantes))
  return(resultado)
}
spotify$duration <- sapply(spotify$duration, convert)
spotify$duration <- as.numeric(spotify$duration)

#REORDENANDO COLUMNAS
spotify <- spotify[, c("artists", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo", "key", "mode" )]

#ELIMINAR AQUELLAS DONDE SE REPITE NOMBRE DE ARTISTA Y CANCION - baja de 169909 a 156608
spotify <- subset(spotify, !duplicated(paste(artists, name)))

#ELIMINAR AQUELLAS DONDE LA DURACION SEA MAYOR A 20 MIN - baja de 156608 a 156353
spotify <- subset(spotify, duration <= 20)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR [', ', y ']
spotify$artists <- gsub("\\['|'|'\\]", "", spotify$artists)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR [", ", y "]
spotify$artists <- gsub("\\[\"|\"\\]", "", spotify$artists)

#PASAR TODO A MINUSCULAS PARA FACILITAR FILTRADO
spotify$artists <- sapply(spotify$artists, tolower)
spotify$name <- sapply(spotify$name, tolower)

#VUELVO A APLICAR EL FILTRO PARA ELIMINAR DUPLICADOS, baja de 156353 a 155976

#ELIMINAR AQUELLAS DONDE LA DURACION SEA MENOR A 1 MIN - baja de 155976 a 154524
spotify <- subset(spotify, duration >= 1)

#CAMBIANDO VALORES DE LA COLUMNA "MODE"
spotify$mode <- ifelse(spotify$mode == 0, "Menor", "Mayor")

#REDONDEAR TEMPO (BPM)
spotify$tempo <- round(spotify$tempo, 0)

#REDONDEAR VALENCE
spotify$valence <- round(spotify$valence, 2)

#REDONDEAR DANCE
spotify$dance <- round(spotify$dance, 2)

#REDONDEAR ACOUST
spotify$acoust <- round(spotify$acoust, 2)

#REDONDEAR LIVENESS
spotify$liveness <- round(spotify$liveness, 2)

#REDONDEAR ENERGY
spotify$energy <- round(spotify$energy, 2)

#REDONDEAR SPEECHNESS
spotify$speech <- round(spotify$speech, 2)

#CAMBIO DE RANGO EN LA VARIABLE LOUDNESS, PARA PASAR DE -60/0 A 40/100, Y REDONDEAR
spotify$loudness <- round(spotify$loudness + 100, 0)

#NO MOSTRAR INSTRUMENTALNESS COMO FORMATIO CIENTIFICO
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

