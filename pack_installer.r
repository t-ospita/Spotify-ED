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
#Ejemplo:
#packages("ggplot2", "tidyverse")