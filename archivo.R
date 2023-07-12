library(readr)
library(ggplot2)
library(dplyr)
library(GGally)
library(ggthemes)
library(patchwork)

#CARGA DE CSV EN VARIABLE SPOTIFY
spotify <- read.csv("data.csv")
------------------------------------------------
#FILTRAR Y ORDENAR TABLA, VALORES, NOMBRES, FORMATOS, ETC
------------------------------------------------
  
#ELIMINAR ALGUNAS VARIABLES
spotify <- subset(spotify, select = -c(id, release_date, key, mode))

#CAMBIAR NOMBRES DE ALGUNAS VARIABLES
spotify <- spotify %>% rename(Artist1 = artists, instr = instrumentalness, 
                              duration = duration_ms, pop = popularity, 
                              dance = danceability, speech = speechiness, acoust = acousticness)

#CORREGIR EL NOMBRE DEL ARTISTA PARA QUITAR SIMBOLOS EXTRAÑOS [', ', '], [", ", y "]
spotify$Artist1 <- gsub("\\['|'|'\\]", "", spotify$Artist1)
spotify$Artist1 <- gsub("\\[\"|\"\\]", "", spotify$Artist1)
spotify$Artist1 <- gsub('"', "", spotify$Artist1)

#PASAR ARTISTA Y CANCION A MINUSCULAS PARA FACILITAR FILTRADO
spotify$Artist1 <- sapply(spotify$Artist1, tolower)
spotify$name <- sapply(spotify$name, tolower)

#REORDENANDO COLUMNAS
spotify <- spotify[, c("Artist1", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo" )]
# 
# spotify <- respaldo2
# respaldo2 <- spotify

#ELIMINAR AQUELLAS DONDE SE REPITE NOMBRE DE ARTISTA Y CANCION - baja de 169909 a 156231
#spotifydup <- subset(spotify, !duplicated(paste(Artist1, name)))
spotify <- spotify %>% group_by(Artist1, name) %>% mutate(dup = n()) %>% arrange(desc(pop)) %>% filter(!(dup > 1 & row_number() > 1)) %>% select(-dup)

#REDONDEAR VARIABLES
spotify$tempo <- round(spotify$tempo, 0)
spotify$valence <- round(spotify$valence, 2)
spotify$dance <- round(spotify$dance, 2)
spotify$acoust <- round(spotify$acoust, 2)
spotify$liveness <- round(spotify$liveness, 2)
spotify$energy <- round(spotify$energy, 2)
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

--------------------------------------
#ANALIZAR Y BORRAR ATÍPICOS  Y OTROS DATOS QUE DETECTEMOS QUE NO CORRESPONDEN
--------------------------------------
  
summary(spotify$duration)
q1 <- quantile(spotify$duration, 0.25)
q3 <- quantile(spotify$duration, 0.75)
rango <- q3 - q1
#inferior <- q1 - (1.5*rango) - da numero negativo, no tomamos este limite (Igual resto menores a 1)
superior <- q3 + (1.5*rango) #da 6.8, bastante parecido al 7 de Tadeo
#vector con valores atípicos
#atipicos <- boxplot.stats(spotify$duration)$out

#ELIMINAR ATIPICOS DE LA TABLA Y MENORES A 1 - pasa de 156231 a 147585
spotify <- subset(spotify, duration <= superior & duration >= 1)
rm(q1, q3, rango, superior)
#BORRAR A ERNEST HEMINGWAY (AUDIOLIBROS EN RUSO) - BAJA DE 147585 A 146371
spotify <- spotify %>% filter(Artist1 != "эрнест хемингуэй")
#BORRAR A ERICH MARIA REMARQUE (AUDIOLIBROS EN RUSO) - BAJA DE XXXXXX A XXXXXX
spotify <- spotify %>% filter(Artist1 != "эрих мария ремарк")
#BORRAR A SEWERYN GOSZCZYNSKI (PODCAST POLACO) - BAJA DE XXXXXX A 145514
spotify <- spotify %>% filter(Artist1 != "seweryn goszczyński")

#CREANDO LA COPIA PARA EL OTRO ANALISIS
spotifyprom <- spotify

-------------------------------------
#ANALISIS DE COMO FUERON CAMBIANDO LAS CANCIONES A LO LARGO DE LOS AÑOS (USANDO "spotifyprom")
-------------------------------------
  
spotifyprom$year <-     ifelse(spotifyprom$year >= 1920 & spotifyprom$year < 1925, 1920,
                        ifelse(spotifyprom$year >= 1925 & spotifyprom$year < 1930, 1925,
                        ifelse(spotifyprom$year >= 1930 & spotifyprom$year < 1935, 1930,
                        ifelse(spotifyprom$year >= 1935 & spotifyprom$year < 1940, 1935,
                        ifelse(spotifyprom$year >= 1940 & spotifyprom$year < 1945, 1940,
                        ifelse(spotifyprom$year >= 1945 & spotifyprom$year < 1950, 1945,
                        ifelse(spotifyprom$year >= 1950 & spotifyprom$year < 1955, 1950,
                        ifelse(spotifyprom$year >= 1955 & spotifyprom$year < 1960, 1955,
                        ifelse(spotifyprom$year >= 1960 & spotifyprom$year < 1965, 1960,
                        ifelse(spotifyprom$year >= 1965 & spotifyprom$year < 1970, 1965,
                        ifelse(spotifyprom$year >= 1970 & spotifyprom$year < 1975, 1970,
                        ifelse(spotifyprom$year >= 1975 & spotifyprom$year < 1980, 1975,
                        ifelse(spotifyprom$year >= 1980 & spotifyprom$year < 1985, 1980,
                        ifelse(spotifyprom$year >= 1985 & spotifyprom$year < 1990, 1985,
                        ifelse(spotifyprom$year >= 1990 & spotifyprom$year < 1995, 1990,
                        ifelse(spotifyprom$year >= 1995 & spotifyprom$year < 2000, 1995,
                        ifelse(spotifyprom$year >= 2000 & spotifyprom$year < 2005, 2000,
                        ifelse(spotifyprom$year >= 2005 & spotifyprom$year < 2010, 2005,
                        ifelse(spotifyprom$year >= 2010 & spotifyprom$year < 2015, 2010,
                        ifelse(spotifyprom$year >= 2015 & spotifyprom$year < 2020, 2015,
                        ifelse(spotifyprom$year >= 2020 & spotifyprom$year < 2023, 2020, 0000)))))))))))))))))))))

#PROMEDIANDO TODAS LAS OBSERVACIONES QUE COMPARTAN YEAR 
varsec <- c("duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")
prom <- aggregate(. ~ year, data = spotifyprom[, c("year", varsec)], FUN = mean)
prom[, varsec] <- lapply(prom[, varsec], round, digits = 2)
prom$tempo <- round(prom$tempo, 0)
prom$pop <- round(prom$pop, 0)
prom$loudness <- round(prom$loudness, 0)
rm(varsec)


p <- prom %>% ggplot(aes(year, acoust)) + geom_path(color = "orange") + geom_point(size = 2) + scale_x_continuous(breaks = c(1940, 1980, 2020)) + 
ylim(0,1) + theme_economist() + ggtitle("Qué tan acústica es una canción según el año:") + 
theme(plot.title = element_text(size = 14))+ xlab(expression(bold("Año"))) + ylab(expression(bold("Nivel de Acústica:"))) + 
theme(axis.title.x = element_text(margin = margin(t = 5)), axis.title.y = element_text(margin = margin (r=5)), plot.margin = margin(25, 25, 25, 25), plot.title = element_text(margin = margin(b = 20)))

ggsave("acoust2.png", plot = p, width = 10, height = 6)

q <- prom %>% ggplot(aes(year, energy)) + geom_path(color = "orange") + geom_point(size = 2) + scale_x_continuous(breaks = c(1940, 1980, 2020)) + 
ylim(0,1) + theme_economist() + ggtitle("Nivel de intensidad y actividad de una canción según el año:") + 
theme(plot.title = element_text(size = 14))+ xlab(expression(bold("Año"))) + ylab(expression(bold("Nivel de Energía:"))) + 
theme(axis.title.x = element_text(margin = margin(t = 5)), axis.title.y = element_text(margin = margin (r=5)), plot.margin = margin(25, 25, 25, 25), plot.title = element_text(margin = margin(b = 20)))

ggsave("energy2.png", plot = q, width = 10, height = 6)

r <- prom %>% ggplot(aes(year, explicit)) + geom_path(color = "orange") + geom_point(size = 2) + scale_x_continuous(breaks = c(1940, 1980, 2020)) + 
ylim(0,1) + theme_economist() + ggtitle("Porcentaje de canciones con lenguaje explícito según el año:") + 
theme(plot.title = element_text(size = 14))+ xlab(expression(bold("Año"))) + ylab(expression(bold("Explicit"))) + 
theme(axis.title.x = element_text(margin = margin(t = 5)), axis.title.y = element_text(margin = margin (r=5)), plot.margin = margin(25, 25, 25, 25), plot.title = element_text(margin = margin(b = 20)))

ggsave("explicit2.png", plot = r, width = 10, height = 6)

s <- prom %>% ggplot(aes(year, instr)) + geom_path(color = "orange") + geom_point(size = 2) + scale_x_continuous(breaks = c(1940, 1980, 2020)) + 
ylim(0,1) + theme_economist() + ggtitle("Nivel de instrumentalidad de una canción según el año:") + 
theme(plot.title = element_text(size = 14))+ xlab(expression(bold("Año"))) + ylab(expression(bold("Instrumentalidad"))) + 
theme(axis.title.x = element_text(margin = margin(t = 5)), axis.title.y = element_text(margin = margin (r=5)), plot.margin = margin(25, 25, 25, 25), plot.title = element_text(margin = margin(b = 20)))

ggsave("instr2.png", plot = s, width = 10, height = 6)

t <- prom %>% ggplot(aes(year, loudness)) + geom_path(color = "orange") + geom_point(size = 2) + scale_x_continuous(breaks = c(1940, 1980, 2020)) + 
ylim(0,100) + theme_economist() + ggtitle("Volumen en decibeles de una canción según el año:") + 
theme(plot.title = element_text(size = 14))+ xlab(expression(bold("Año"))) + ylab(expression(bold("Volumen en decibeles"))) + 
theme(axis.title.x = element_text(margin = margin(t = 5)), axis.title.y = element_text(margin = margin (r=5)), plot.margin = margin(25, 25, 25, 25), plot.title = element_text(margin = margin(b = 20)))

ggsave("loudness2.png", plot = t, width = 10, height = 6)

mix <- (p + q) / (r + s)
ggsave("zmix2.png", plot = mix, width = 20, height = 10)

rm(p, q, r, s, t, mix, zmix)
-------------------------------------


-------------------------------------
#ANALISIS DE LOS ARTISTAS MAS POPULARES (USANDO "spotify")
-------------------------------------
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
spotify <- spotify[, c("Artist1", "Artist2", "Artist3", "name", "year", "duration", "explicit", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")]
respaldo <- spotify
#BORRAR DE ARTIST1 EL RESTO DE ARTISTAS QUE NO QUIERO
spotify$Artist1 <- sub(",.*", "", spotify$Artist1)
rm(col_name, i, maxnom, separar_Artist1)

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

filtrados1 <- subset(spotify, Artist1 %in% validos, select = -c(Artist2, Artist3))
filtrados2 <- subset(spotify, Artist2 %in% validos, select = -c(Artist1, Artist3))
filtrados3 <- subset(spotify, Artist3 %in% validos, select = -c(Artist1, Artist2))

names(filtrados2)[names(filtrados2) == "Artist2"] <- "Artist1"
names(filtrados3)[names(filtrados3) == "Artist3"] <- "Artist1"

filtrados <- rbind(filtrados1, filtrados2, filtrados3)
rm(filtrados1, filtrados2, filtrados3)

#SI QUIERO SOLAMENTE UNA LINEA POR CADA ARTISTA, PROMEDIANDO TODAS SUS CANCIONES
#pops <- aggregate(. ~ Artist1, data = filtrados[, c("Artist1", varsec)], FUN = mean)
#SI QUIERO UNA LINEA POR ARTISTA Y POR AÑO (17162 OBSERVACIONES - ME TRAE A BEETHOVEN COMO EL MÁS POPULAR DEL 2007, CON UNA SOLA CANCION)
#pops <- aggregate(. ~ Artist1 + year, data = filtrados[, c("Artist1", varsec)], FUN = mean)

#SI QUIERO UNA LINEA POR ARTISTA Y POR AÑO PARA AQUELLOS QUE TENGAN MÁS DE 3 CANCIONES EN UN AÑO, Y BORRAR EL RESTO (8256 OBSERVACIONES) 
art3 <- filtrados %>% group_by(Artist1, year) %>% filter(n() >= 3)
pops <- aggregate(. ~ Artist1 + year, data = art3[, c("Artist1", varsec)], FUN = mean)
pops[, varsec] <- lapply(pops[, varsec], round, digits = 2)
pops$year <- round(pops$year, 0)
pops$tempo <- round(pops$tempo, 0)
pops$pop <- round(pops$pop, 0)
pops$loudness <- round(pops$loudness, 0)
rm(freq1, validos)

#pops$decada <- NA
#pops$decada <- floor(pops$year / 10) * 10
#pops$decada <- as.factor(pops$decada)
pops <- pops[, c("Artist1", "year", "duration", "pop", "dance", "instr", "acoust", "speech", "liveness", "energy", "loudness", "valence", "tempo")]

--------------------------------------
max_pop <- aggregate(pop ~ year, data = pops, FUN = max)

# Mostrar los resultados como texto
for (i in 1:nrow(max_pop)) {
  año <- max_pop$year[i]
  max_valor <- max_pop$pop[i]
  artist <- pops$Artist1[pops$year == año & pops$pop == max_valor]
  cat("El artista más popular de", año, "es", artist, "\n")
}

#TABLA CON LOS MÁS POPULARES DESDE EL 2000: 
popsxaño <- pops %>% group_by(year) %>% filter(year >= 2000)
popsxaño <- aggregate(. ~ Artist1, data = popsxaño[, c("Artist1", varsec)], FUN = mean)
popsxaño$tempo <- round(popsxaño$tempo, 0)
popsxaño$valence <- round(popsxaño$valence, 2)
popsxaño$dance <- round(popsxaño$dance, 2)
popsxaño$acoust <- round(popsxaño$acoust, 2)
popsxaño$liveness <- round(popsxaño$liveness, 2)
popsxaño$energy <- round(popsxaño$energy, 2)
popsxaño$speech <- round(popsxaño$speech, 2)
popsxaño$instr <- round(popsxaño$instr, 2)
popsxaño$loudness <- round(popsxaño$loudness, 2)
popsxaño$pop <- round(popsxaño$pop, 0)
popsxaño$year <- round(popsxaño$year, 0)
popsxaño$duration <- round(popsxaño$duration, 2)

#GRAFICA DE LOS MAS POPULARES DESDE EL 2000:
graf <- popsxaño %>% filter(pop >= 70) %>% ggplot(aes(x = pop, y = reorder(Artist1, pop))) + geom_bar(stat = "identity") + 
  labs(x = "", y = "") + theme_economist() + theme(plot.title = element_text(margin = margin(b = 20))) + xlim(0.0, 100.0) + ggtitle("Los artistas más populares del siglo XXI:")
graf

--------------------------------------
ggcorr(popsxaño[, vv])



  

-------------------------------------
#ANALIZAR ARTISTAS SIMILARES A OTROS:
artsimil <- aggregate(. ~ Artist1, data = spotify[, c("Artist1", varsec)], FUN = mean)

vv <- 3:13
distancias <- as.matrix(dist(artsimil[, vv]))
artista <- which((artsimil$Artist1 == "the beatles"))

similares <- data.frame(
  Artist1 = artsimil$Artist1,
  distancia_to_bb = distancias[, artista]
)

similares$dd <- with(similares, reorder(Artist1, distancia_to_bb))

ggplot(subset(similares, subset = rank(distancia_to_bb) < 11)) + 
  geom_point(aes(distancia_to_bb, dd), size = 2) + theme_economist() + xlab("") + ylab("")
  theme(axis.title.y = element_text(size = I(15))) + ggtitle("Artistas más similares a The Beatles:")
  

