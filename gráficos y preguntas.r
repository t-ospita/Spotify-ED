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

packages("readr", "ggplot2", "dplyr", "tidyverse", "ggthemes", "ggpointdensity",
        "cowplot", "reshape2", "shadowtext", "RColorBrewer")

#Cargando datos
spotify <- read.csv("clean_data.csv")
spotify <- spotify[order(spotify$pop, decreasing = TRUE),]

#*1. De qué factores depende la popularidad

#! GRAFICO DE CADA VARIABLE CON POP
variables <- names(spotify)[sapply(spotify, is.numeric)]
plot_list <- list()
num_elements <- length(variables) - 1
windowsFonts(Poppins = windowsFont("Poppins"))
#Definiendo el foor loop
for (i in seq_along(variables)[-num_elements]) { #seq_along es usado para después poder nombrar los gráficos
  var <- variables[i] #Toma el elemento i de Variables

  p  <- ggplot(spotify, aes(x = .data[[var]], y = pop)) +
  geom_pointdensity(size = 2, method = "kde2d") +
  scale_color_viridis_c(name = "", guide = "none") + #Hay que quitar guide porque plot_grid automaticamente reduce la escala del eje x
  theme_minimal() +
    theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'),
    text=element_text(family="Poppins", face = "bold", colour = "#453f76", size = 15),
    axis.line = element_line(colour = "#453f76"),
    axis.text.x = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10),
    axis.text.y = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10)
    )
  #theme(text=element_text(family="Poppins", face = "bold", colour = "#453f76"), 
    #panel.grid.major = element_line(colour =  "#453f76",
    # panel.border = element_line(colour = "#453f76", 
    #plot.background = element_blank())))
    #)

  plot_name <- paste0("comparacion_de_", var, "_con_pop.png") #Creando el nombre del gráfico i
  #ggsave(file.path("plot",, "comparacion", plot_name), plot = p) #Guardando cada gráfico de forma separadai:

  plot_list[[i]] <- p #Guardando en una lista para intentar graficar todos los plots juntos
}
plot_list[[4]]
ggsave('test.png', plot_list[[4]], bg = "transparent")

#Guardando el gráfico (!DEMORA MUCHO EN GRAFICAR)
plot_grid(plotlist = plot_list)
ggsave(file.path("plot", "comparacion_popularidad_vs_todas_las_variables.png"))


#!HEATMAP
spotifysindec <- subset(spotify, select = -decada)
heat <- cor(spotifysindec[sapply(spotifysindec, is.numeric)])
#Cambiando formato al adecuado
heat <- melt(heat)
#Gráfico para ver las relaciones entre variables
heatmap_spotify <-
ggplot(heat, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("#544e8f","#dedede","#D0F87E")) +
    #geom_text(aes(label = round(value, 2)), color = "#2A2747") +
    theme_minimal()+
    theme(text=element_text(family="Poppins", face = "bold", colour = "#453f76", size = 15), 
    axis.line = element_line(colour = "#453f76"),
    axis.text.x = element_text(family="Poppins", colour = "#453f76", size = 10),
    axis.text.y = element_text(family="Poppins", colour = "#453f76", size = 12)) +
    labs(x = "", y = "", fill = "Coef. de correlación") +
    coord_fixed(ratio = 1)+
    scale_x_discrete(guide=guide_axis(n.dodge = 2))

ggsave(file.path("plot", "heatmap.png"))


#¿Cuáles son las características de las canciones  según el año de lanzamiento? 
#¿Cómo fueron evolucionando esas características?

#La idea es:
#Analizar el promedio de cada variable en cada año, y graficar para analizar como cambia.

#Hay que calcular las medias de cada variable en funcion del año
#Tomemos las variables numéricas
num <- sapply(spotify, is.numeric)
spotifyprom <- spotify[, num]

#Escalando los valores de cada variable al rango [0,1]
scale_val <- function(x){(x-min(x))/(max(x)-min(x))}
variable_names <- c("loudness", "tempo", "duration")
for (var in variable_names) {
    spotifyprom[[var]] <- scale_val(spotify[[var]])
}
spotifyprom$pop <- spotifyprom$pop / 100

#Calculando los promedios
spotifyprom <- aggregate(. ~ year, data = spotifyprom, FUN = mean)

#Separando las que se relacion positiva y negativamente con año.
#! Notar: se podría separar con un loop que vea el signo de la cor (no hay tiempo para testear)
#Positiva:
spotifyposcor <- spotifyprom %>% 
    select(year, pop, energy, loudness, tempo, dance) %>%
    gather(key = "variable", value = "value", -year)
#Negativa:
spotifynegcor <- spotifyprom %>% 
    select(year, acoust, instr, valence) %>%
    gather(key = "variable", value = "value", -year)
#Especiales
spotifypopcor <- spotifyprom %>% 
    select(year, pop) %>%
    gather(key = "variable", value = "value", -year)



#Haciendo gráficos:
posandneg <- list(spotifyposcor, spotifynegcor)
plots_posneg <- list()
for (i in seq_along(posandneg)) {
    df <- posandneg[[i]]
    p <- ggplot(df, aes(x = year, y = value)) +
        geom_line(aes(color = variable), size = 2) +
        scale_color_manual(values = c("#2A2747", "#0800C0", "#8D0081", "#007B88", "#664600" )) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            #legend.background = element_rect(fill='transparent'),
            #legend.box.background = element_rect(fill='transparent'),
            text=element_text(family="Poppins", face = "bold", colour = "#453f76", size = 15),
            axis.line = element_line(colour = "#453f76"),
            axis.text.x = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10),
            axis.text.y = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10),
            legend.key.size = unit(2, "cm"),
            legend.title = element_text(size = 21, face = "bold"),
            legend.text = element_text(size = 18.3),
            ) +
        labs(x = "Año", y = "Valor")+
        scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000, 2020))    
    
    plots_posneg[[i]] <- p
}
plot_grid(plotlist = plots_posneg, ncol = 1, nrow = 2)
ggsave(file.path("plot", "promedio_variables_año.png"))


#! Gráfica de pop y year

ggplot(spotifypopcor, aes(x = year, y = value)) +
        geom_line(aes(color = variable), size = 2) +
        scale_color_manual(values = c("#2A2747", "#0800C0", "#8D0081", "#007B88", "#664600" )) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            #legend.background = element_rect(fill='transparent'),
            #legend.box.background = element_rect(fill='transparent'),
            text=element_text(family="Poppins", face = "bold", colour = "#453f76", size = 15),
            axis.line = element_line(colour = "#453f76"),
            axis.text.x = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10),
            axis.text.y = element_text(family="Poppins", face = "bold", colour = "#453f76", size = 10),
            legend.key.size = unit(2, "cm"),
            legend.title = element_text(size = 21, face = "bold", color = "transparent"),
            legend.text = element_text(size = 18.3),
            ) +
        labs(x = "Año", y = "Valor")+
        scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000, 2020))

ggsave(file.path("plot", "promedio_pop_year.png"))
