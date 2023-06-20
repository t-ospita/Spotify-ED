# Spotify-ED

Análisis del dataset sobre datos de canciones de Spotify desde 1921 a 2020, buscando responder las siguientes preguntas:
* ¿De qué factores depende la popularidad de una canción?
* ¿Cuáles son los artistas con mayor cantidad de canciones de alta popularidad?
* ¿Cuáles son las características de las canciones (valence, tempo, instrumentalness, danceability, energy) según el año de lanzamiento? ¿Cómo fueron evolucionando esas características?
 
## Dataset utilizado:
[Spotify Data 1921-2020](https://www.kaggle.com/datasets/ektanegi/spotifydata-19212020)
> The "data.csv" file contains more than 160.000 songs collected from Spotify Web API. The dataset is from Spotify and contains 169k songs from the year 1921 to year 2020. Each year got
top 100 songs.

### Variables:
* _Track_id_: el ID que utiliza Spotify para cada canción. 
* _Artists_: nombres de los artistas de cada canción. Si hay más de uno se separan con “;”. 
* _Name_: nombre de la canción. 
* _Year_: año de lanzamiento de la canción. 
* _Release_date_: fecha de lanzamiento de la canción, en formato YYYY-MM-DD.
* _Duration_ms_: la duración en milisegundos de cada canción. 
* _Danceability_: describe cuán bailable es una canción basada en una combinación de elementos musicales, que incluyen tempo, la estabilidad del ritmo, la fuerza del beat, y la regularidad promedio. Se valora de 0 a 1; cuanto más cercano a 1, más bailable la canción.
* _Energy_: mide el nivel de intensidad y actividad de la canción, valorado de 0 a 1. Cuanto más cercano a 1, la canción se siente más rápida, ruidosa o potente.  
* _Key_: es el tono promedio estimado de la canción. Los números enteros se traducen utilizando notación estándar de tono, 0 = C, 1 = C♯/D♭, 2 = D, y así sucesivamente. Si no se detecta un tono el valor es -1.
* _Loudness_: mide el volumen promedio de una canción en decibeles. Los valores del volumen son promediados a través de toda la canción y son útiles para comparar el volumen relativo de cada una. Los valores varían típicamente entre -60 y 0 db.
* _Mode_: indica la modalidad (mayor o menor) de una canción. Mayor se representa como 1 y menor como 0.
* _Speechiness_: mide la cantidad de palabras habladas en la canción, valorada de 0 a 1. Cuanto más cerca de 1, es más probable que la canción sea hecha completamente de palabras habladas (por ejemplo un podcast tendría puntuación de 1).
* _Acousticness_: indica si la canción es acústica o no, valorada de 0 a 1. Cuanto más cercano a 1,  mayor probabilidad de que la canción sea acústica.
* _Instrumentalness_: mide el nivel instrumental de la pista, valorado de 0 a 1. Los sonidos "ooh" y "aah" se tratan como instrumentales en este contexto. Las canciones de rap o de palabras habladas son claramente "vocales". Cuanto más cercano a 1 esté el nivel de instrumentalidad, mayor será la probabilidad de que la pista no contenga contenido vocal.
* _Liveness_: mide la presencia de audiencia en la canción, valorada de 0 a 1. Cuanto más cercano a 1, es más probable que la canción haya sido grabada en vivo.
* _Valence_: mide que tan alegre es una canción, valorada de 0 a 1. Cuanto más cerca de 1, es más probable que la canción suene más positiva o alegre. Valores más bajos indican que la canción suena más negativa (enojada, triste, etc). 
* _Tempo_: mide en beats por minuto la velocidad o el ritmo de una canción. Su rango habitual se encuentra entre 50 y 150 BPM. 
* _Popularidad_: mide el nivel de popularidad de la canción, valorada de 0 a 100. Para obtener ese valor, se toma en cuenta la cantidad de reproducciones de cada canción y que tan recientes han sido. 
 * _Explicit_: variable categórica que indica con 0 si la canción no contiene letras explícitas, y con 1 si la canción contiene dichas letras. 

# Equipo : gatito explosion.jpg  <img src= "https://media.discordapp.net/attachments/903024711485775882/1118194764735582329/gato.jpg" width = "50" height = "50" />
* [evilla8](https://github.com/evilla8)
* [Montel7u]([link](https://github.com/Montel7u))
* [Mathias](https://github.com/mathitorres1)
* [user](link)

