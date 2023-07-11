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
          "tidyverse", "tibble", "reshape2",
          "hrbrthemes", "cowplot", "ggpointdensity",
          "data.table", "janitor"))

#Función para guardar gráfico:
plot_save <- function(plot_objeto, filename) {
  file_path <- file.path("plot", "limpieza", filename)
  ggsave(file_path, plot = plot_objeto)
}

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
box_atipicos <- ggplot(spotify, aes(x = duration, y = duration)) +
                geom_boxplot()
#Guardando el gráfico
plot_save(box_atipicos, "boxplot_duracion_atipicos.png")

#Extrayendo átipicos del gráfico
atipico_min <- min(boxplot.stats(spotify$duration)$out)
atipicos <- boxplot.stats(spotify$duration)$out

#Creando una nueva columna con información sobre atípico (out = outliner)
spotify$out <- ifelse(spotify$duration > atipico_min, "yes", "no")
spotify$out <- as.factor(spotify$out)

#Gráficos para analizar los atípicos
box_ordenado_out <-
ggplot(spotify, aes(x = out, y = pop, color = out)) +
  geom_boxplot() +
  labs(x = "¿Atípico?", y = "Popularidad") +
  stat_summary(fun = mean, color = "black", shape = 10)
histograma_ordenado_out <-
ggplot(spotify, aes(x = pop, fill = out)) +
  geom_histogram() +
  labs(x = "Popularidad", fill = "Atípico")
#Guardando gráficos
plot_save(box_ordenado_out, "boxplot_popularidad_atipico.png")
plot_save(histograma_ordenado_out, "histograma_popularidad_atipicos.png")
#!explicit a factor (1 si, 0 no)
spotify$explicit <- ifelse(spotify$explicit == 0, "No", "Yes")
spotify$explicit <- as.factor(spotify$explicit)
#!mode a factor (1 mayor, 0 menor)
spotify$mode <- ifelse(spotify$mode == 0, "Menor", "Mayor")
spotify$mode <- as.factor(spotify$mode)

#!Eliminar id y release_date (ya tenemos el año)
spotify <- subset(spotify, select = -c(release_date))

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

#Eliminar duplicados
col_og <- nrow(spotify)
spotify <- anti_join(spotify, get_dupes(spotify)) #Notar que los duplicados fueron eliminados así porque duplicated traía problemas
#Restando cantidad de observaciones viejas con cantidad de observaciones nuevas
col_og - nrow(spotify)
#cambio de rango en la variable loudness, para pasar de -60/0 a 40/100, y redondear
spotify$loudness <- round(spotify$loudness + 100, 0)

#no mostrar instrumentalness como formatio cientifico
spotify$instr <- format(spotify$instr, scientific = FALSE)
spotify$instr <- as.numeric(spotify$instr)

#! Creando columnas con décadas
spotify$decada <- NA
spotify$decada <- floor(spotify$year / 10) * 10
spotify$decada <- as.factor(spotify$decada)

#! Creando un heatmap de todas las variables para analizar relacion
#Calculando matriz de correlación
heat <- cor(spotify[sapply(spotify, is.numeric)])
#Cambiando formato al adecuado
heat <- melt(heat)

#Gráfico para ver las relaciones entre variables
heatmap_spotify <-
ggplot(heat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "black") +
  scale_fill_gradient(high = "#3246f8", low = "#d6d6d6") +
  geom_text(aes(label = round(value, 2)), color = "black")
plot_save(heatmap_spotify, "heatmap_variables.png")
#!Haciendo gráfico de puntos de todas las variables con popularidad
#El objetivo es usar un for loop que itere a través de todas las variables cambiando el eje x

variables <- names(spotify)[sapply(spotify, is.numeric)]
plot_list <- list()
#Definiendo el foor loop
for (i in seq_along(variables)) { #seq_along es usado para después poder nombrar los gráficos
  var <- variables[i] #Toma el elemento i de Variables

  p  <- ggplot(spotify, aes(x = .data[[var]], y = pop)) +
  geom_pointdensity(size = 2, method = "kde2d") +
  scale_color_viridis_c(name = "", guide = "none") + #Hay que quitar guide porque plot_grid automaticamente reduce la escala del eje x
  theme_minimal()
  
  plot_name <- paste0("comparacion_de_", var, "_con_pop.png") #Creando el nombre del gráfico i
  ggsave(file.path("plot", "limpieza", "comparacion", plot_name), plot = p) #Guardando cada gráfico de forma separadai:

  plot_list[[i]] <- p #Guardando en una lista para intentar graficar todos los plots juntos
}

#Guardando el gráfico (!DEMORA MUCHO EN GRAFICAR)
plot_grid(plotlist = plot_list)
ggsave(file.path("plot", "limpieza", "comparacion_popularidad_vs_todas_las_variables.png"))

#! Notar atípicos: -tempo 0 y mucha pop, -dance 0 y mucha pop.
#Hay que ver que son esas "canciones"
spotify0 <- subset(spotify, tempo == 0 & pop > 50)
head(spotify0, 10)
nrow(spotify0)
spotify01 <- subset(spotify, dance == 0 & pop > 50)
head(spotify01, 10)
nrow(spotify01)
identical(spotify0, spotify01)

#Se puede concluir que, los que tienen mayor popularidad, son ruido o no son canciones como tal
#Eliminar todas las "canciones" con dance == 0
spotify <- subset(spotify, tempo != 0 )
col_og - nrow(spotify) #Eliminamos 13817 canciones
nrow(subset(spotify, dance == 0)) #Chequeando que se hayan eliminado las canciones con dance = 0

#Guardando el dataframe en un nuevo .csv

fwrite(spotify, "clean_data.csv")
#* Queda todo listo para analizar los datos