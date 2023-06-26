library(readr)
library(ggplot2)
library(dplyr)
library(GGally)

#CARGA DE CSV EN VARIABLE SPOTIFY
spotify <- read.csv("data.csv")

#ELIMINAR ALGUNAS VARIABLES
spotify <- subset(spotify, select = -c(id, release_date))

#CAMBIAR NOMBRES DE ALGUNAS VARIABLES
spotify <- spotify %>% rename(Artist1 = artists, instr = instrumentalness, 
                              duration = duration_ms, pop = popularity, 
                              dance = danceability, speech = speechiness, acoust = acousticness)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR [', ', y ']
spotify$Artist1 <- gsub("\\['|'|'\\]", "", spotify$Artist1)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR [", ", y "]
spotify$Artist1 <- gsub("\\[\"|\"\\]", "", spotify$Artist1)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR "
spotify$Artist1 <- gsub('"', "", spotify$Artist1)

#PASAR TODO A MINUSCULAS PARA FACILITAR FILTRADO
spotify$Artist1 <- sapply(spotify$Artist1, tolower)
spotify$name <- sapply(spotify$name, tolower)

#ELIMINAR AQUELLAS DONDE SE REPITE NOMBRE DE ARTISTA Y CANCION - baja de 169909 a 156231
spotify <- subset(spotify, !duplicated(paste(Artist1, name)))

#REORDENANDO COLUMNAS
spotify <- spotify[, c("Artist1", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo", "key", "mode" )]

#CAMBIANDO VALORES DE LA COLUMNA "MODE"
spotify$mode <- ifelse(spotify$mode == 0, "Menor", "Mayor")
spotify$mode <- as.factor(spotify$mode)

#CAMIANDO VALORES DE LA COLUMNA "EXPLICIT"
spotify$explicit <- ifelse(spotify$explicit == 0, "No", "Si")
spotify$explicit <- as.factor(spotify$explicit)

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
rm(convert)
-------------------------------
  
summary(spotify$duration)
q1 <- quantile(spotify$duration, 0.25)
q3 <- quantile(spotify$duration, 0.75)
rango <- q3 - q1
#inferior <- q1 - (1.5*rango) - da numero negativo, no tomamos este limite (Igual resto menores a 1)
superior <- q3 + (1.5*rango) #da 6.8, bastante parecido al 7 de Tadeo
#vector con valores atípicos
#atipicos <- boxplot.stats(spotify$duration)$out

#ELIMINAR ATIPICOS DE LA TABLA - pasa de 156231 a 149037
spotify <- subset(spotify, duration <= superior)

#ELIMINAR MENORES A 1 MINUTO - pasa de 149037 a 147585
spotify <- subset(spotify, duration >= 1)
rm(q1, q3, rango, superior)
-------------------------------------

#NUEVA COLUMNA SOBRE DATO ATÍPICO
#spotify$out <- ifelse(spotify$duration > superior, "Si", "No")
#spotify$out <- as.factor(spotify$out)

#GRAFICA COMPARACION POP Y ATIPICO
#ggplot(spotify, aes(x = out, y = pop, color = out)) +
  #geom_boxplot()+
  #labs(x = "¿Atípico?", y = "Popularidad")
#ggplot(spotify, aes(x=pop, fill = out)) +
  #geom_histogram() +
  #labs(x = "Popularidad", fill = "Atípico")

#OTRO GRAFICO (NO ME CARGÓ POR LA CANTIDAD DE DATOS)
#spotify %>% ggplot(aes(duration, pop)) + geom_point() + facet_grid(. ~ out)

-----------------------------
#SEPARAR EN N COLUMNAS LOS N ARTISTAS DE CADA CANCION. SI ES UN ARTISTA SOLO, QUE MUESTRE NA EN EL RESTO 
separar_Artist1 <- function(Artist1) {
  sep <- strsplit(Artist1, ", ")
  if (length(sep[[1]]) > 1) {
    return(c(sep[[1]][1], sep[[1]][-1]))
  } else {
    return (c(Artist1, NA))
  }
}
#MAXIMO DE NOMBRES A SEPARAR EN ARTISTS
maxnom <- max(sapply(spotify$Artist1, function (x) length(strsplit(x, ", ")[[1]])))

#CREAR COLUMNAS ADICIONALES
for (i in 2:maxnom) {
if (i <= 3) {
 col_name <- paste0("Artist", i)
 spotify[[col_name]] <- sapply(spotify$Artist1, function(x) separar_Artist1(x)[i])
}
}

#REORDENAR COLUMNAS NUEVAMENTE
spotify <- spotify[, c("Artist1", "Artist2", "Artist3", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo", "key", "mode" )]

#BORRAR DE ARTIST1 EL RESTO DE ARTISTAS QUE NO QUIERO
spotify$Artist1 <- sub(",.*", "", spotify$Artist1)
rm(col_name, i, maxnom, separar_Artist1)

-----------------------------------------------------------
#TOMANDO SOLO ARTIST1

#length(unique(spotify$Artist1)) - DA 19316 ASI QUE HAY 19316 ARTISTAS DIFERENTES EN ARTIST1
#NUEVA TABLA CON LOS PROMEDIOS DE LAS CANCIONES DE CADA ARTISTA
#varsec <- c("year", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")
#freq <- table(spotify$Artist1)
#validos <- names(freq[freq >= 20])
#filtrados <- spotify[spotify$Artist1 %in% validos, ] #EN FILTRADOS QUEDAN SOLAMENTE LOS ARTISTAS QUE TIENEN MAS DE "FREQ" CANCIONES EN "SPOTIFY"
#prom <- aggregate(. ~ Artist1, data = filtrados[, c("Artist1", varsec)], FUN = mean)
#prom[, varsec] <- lapply(prom[, varsec], round, digits = 2)
#prom$year <- round(prom$year, 0)
#prom$tempo <- round(prom$tempo, 0)
#prom$pop <- round(prom$pop, 0)
#prom$loudness <- round(prom$pop, 0)
----------------------------------------------------------
  
#TOMANDO SOLO ARTIST2

#length(unique(spotify$Artist2)) - DA 7747 ASI QUE HAY 7747 ARTISTAS DIFERENTES EN ARTIST1
#NUEVA TABLA CON LOS PROMEDIOS DE LAS CANCIONES DE CADA ARTISTA
# varsec <- c("year", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")
# freq <- table(spotify$Artist2)
# validos <- names(freq[freq >= 20])
# filtrados <- spotify[spotify$Artist2 %in% validos, ] #EN FILTRADOS QUEDAN SOLAMENTE LOS ARTISTAS QUE TIENEN MAS DE "FREQ" CANCIONES EN "SPOTIFY"
# prom <- aggregate(. ~ Artist2, data = filtrados[, c("Artist2", varsec)], FUN = mean)
# prom[, varsec] <- lapply(prom[, varsec], round, digits = 2)
# prom$year <- round(prom$year, 0)
# prom$tempo <- round(prom$tempo, 0)
# prom$pop <- round(prom$pop, 0)
# prom$loudness <- round(prom$pop, 0)

--------------------------------------------------------------
#TOMANDO TANTO ARTIST1 COMO ARTIST2 - EN TOTAL SON 24004 QUE APARECEN AL MENOS UNA VEZ EN ARTIST1 O ARTISTS2
  
# varsec <- c("year", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")
# freq1 <- table(spotify$Artist1)
# freq2 <- table(spotify$Artist2)
# #AGREGAR LAS FREQ DE ARTIST2 A FREQ1
# for (artist in names(freq2)) {
#   if (artist %in% names(freq1)) {
#     freq1[artist] <- freq1[artist] + freq2[artist]
#   } else {
#     freq1[artist] <- freq2[artist]
#   }
# }
# rm(freq2, artist)
# validos <- names(freq1[freq1 >= 20])
# 
# filtrados1 <- spotify[spotify$Artist1 %in% validos, ]
# filtrados1 <- subset(filtrados1, select = -c(Artist2, Artist3))
# 
# filtrados2 <- spotify[spotify$Artist2 %in% validos, ]
# filtrados2 <- subset(filtrados2, select = -c(Artist1, Artist3))
# 
# names(filtrados2)[names(filtrados2) == "Artist2"] <- "Artist1"
# 
# filtrados <- rbind(filtrados1, filtrados2)
# rm(filtrados1, filtrados2)
# 
# prom <- aggregate(. ~ Artist1, data = filtrados[, c("Artist1", varsec)], FUN = mean)
# prom[, varsec] <- lapply(prom[, varsec], round, digits = 2)
# prom$year <- round(prom$year, 0)
# prom$tempo <- round(prom$tempo, 0)
# prom$pop <- round(prom$pop, 0)
# prom$loudness <- round(prom$loudness, 0)

----------------------------------------------
#TOMANDO TANTO ARTIST1, ARTIST2, ARTIST3 - EN TOTAL SON XXXX QUE APARECEN AL MENOS UNA VEZ EN ARTIST1 O ARTISTS2

varsec <- c("year", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")
freq1 <- table(spotify$Artist1)
freq2 <- table(spotify$Artist2)
freq3 <- table(spotify$Artist3)

#AGREGAR LAS FREQ DE ARTIST3 A FREQ2
for (artist in names(freq3)) { 
  if (artist %in% names(freq2)) {
    freq2[artist] <- freq2[artist] + freq3[artist]
  } else {
    freq2[artist] <- freq3[artist]
  }
}
rm(freq3)

#AGREGAR LAS FREQ DE ARTIST2 A FREQ1
for (artist in names(freq2)) {
  if (artist %in% names(freq1)) {
    freq1[artist] <- freq1[artist] + freq2[artist]
  } else {
    freq1[artist] <- freq2[artist]
  }
}
rm(freq2, artist)
validos <- names(freq1[freq1 >= 20])

filtrados1 <- spotify[spotify$Artist1 %in% validos, ]
filtrados1 <- subset(filtrados1, select = -c(Artist2, Artist3))

filtrados2 <- spotify[spotify$Artist2 %in% validos, ]
filtrados2 <- subset(filtrados2, select = -c(Artist1, Artist3))

filtrados3 <- spotify[spotify$Artist3 %in% validos, ]
filtrados3 <- subset(filtrados3, select = -c(Artist1, Artist2))

names(filtrados2)[names(filtrados2) == "Artist2"] <- "Artist1"
names(filtrados3)[names(filtrados3) == "Artist3"] <- "Artist1"

filtrados <- rbind(filtrados1, filtrados2, filtrados3)
rm(filtrados1, filtrados2, filtrados3)

prom <- aggregate(. ~ Artist1, data = filtrados[, c("Artist1", varsec)], FUN = mean)
prom[, varsec] <- lapply(prom[, varsec], round, digits = 2)
prom$year <- round(prom$year, 0)
prom$tempo <- round(prom$tempo, 0)
prom$pop <- round(prom$pop, 0)
prom$loudness <- round(prom$loudness, 0)
rm(filtrados, freq1, validos, varsec)

prom$decada <- NA
prom$decada <- floor(prom$year / 10) * 10
prom$decada <- as.factor(prom$decada)
prom$year <- NULL
prom <- prom[, c("Artist1", "decada", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")]

--------------------------------------
#SEPARAR DATA FRAME POR DECADA 
prom2010_2020 <- prom %>% filter(year >= 2010)
prom2000_2009 <- prom %>% filter(year >= 2000 & year < 2010)
prom1990_1999 <- prom %>% filter(year >= 1990 & year < 2000)
prom1980_1989 <- prom %>% filter(year >= 1980 & year < 1990)
prom1970_1979 <- prom %>% filter(year >= 1970 & year < 1980)
prom1960_1969 <- prom %>% filter(year >= 1960 & year < 1970)
prom1950_1959 <- prom %>% filter(year >= 1950 & year < 1960)
prom1940_1949 <- prom %>% filter(year >= 1940 & year < 1950)
prom1930_1939 <- prom %>% filter(year >= 1930 & year < 1940)
prom1920_1929 <- prom %>% filter(year >= 1920 & year < 1930)
rm(prom)

-------------------------
#AHORA SÍ PODRIA BUSCAR LOS ARTISTAS SIMILARES A OTRO YA QUE EN PROM QUEDÓ UNA LINEA POR ARTISTA
vv <- 3:13
ggcorr(prom[, vv])

distancias <- as.matrix(dist(prom[, vv]))
dim(distancias)                        

badbunny <- which((prom$Artist1 == "maroon 5"))

bbsimilares <- data.frame(
  Artist = prom$Artist1,
  distancia_to_bb = distancias[, badbunny]
)

bbsimilares$dd <- with(bbsimilares, reorder(Artist, distancia_to_bb))

ggplot(subset(bbsimilares, subset = rank(distancia_to_bb) < 11)) + geom_point(aes(distancia_to_bb, dd)) + 
  labs(y = "") + theme(axis.title.y = element_text(size = I(15)))

--------------------------------------
#CAMBIO DE NOMBRES EN COLUMNA YEAR
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

#TODO ESTO ME CREÓ 40 COLUMNAS DE ARTISTAS YA QUE HAY UNA CANCION CON 40 ARTISTAS
#SEPARAR EN N COLUMNAS LOS N ARTISTAS DE CADA CANCION. SI ES UN ARTISTA SOLO, QUE MUESTRE NA EN EL RESTO 
#separar_artists <- function(artists) {
  #sep <- strsplit(artists, ", ")
  #if (length(sep[[1]]) > 1) {
    #return(sep[[1]][-1])
  #} else {
    #return (NA)
  #}
#}
#MAXIMO DE NOMBRES A SEPARAR EN ARTISTS
#maxnom <- max(sapply(spotify$artists, function (x) length(strsplit(x, ", ")[[1]])))
#CREAR COLUMNAS ADICIONALES
#for (i in 2:maxnom) {
  #col_name <- paste0("artist", i)
  #spotify[[col_name]] <- sapply(spotify$artists, function(x) separar_artists(x)[i-1])
#}

--------------------------------
#CUAL ES MAS POPULAR POR CADA DECADA
# prom %>% filter(year >= 1920 & year < 1930) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1930 & year < 1940) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1940 & year < 1950) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1950 & year < 1960) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1960 & year < 1970) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1970 & year < 1980) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1980 & year < 1990) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 1990 & year < 2000) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 2000 & year < 2010) %>% slice_max(order_by = pop) %>% pull(Artist1)
# prom %>% filter(year >= 2010 & year < 2020) %>% slice_max(order_by = pop) %>% pull(Artist1)
# --------------------------------------------------