#- código usado en index.qmd
#- es el código que uso en mi trabajo

#paquetes
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)


#datos
partidos <- read.csv("./assets/LaLiga_Matches.csv")
jugadores <- read.csv("./assets/Big5-statsStats22-23.csv")

#tenemos que arreglar los datos ya que vienen en bruto (incluyen todos los partidos y todos los jugadores)

#primero filtramos el bruto por la temporada 22-23 y los resultados de los partidos del FC Barcelona

barcelona_total <- partidos %>% 
  filter(Season=="2022-23")%>%
  filter(HomeTeam=="Barcelona" | AwayTeam=="Barcelona")

#Una vez hecho modificamos el nombre de las columnas y las pasamos al castellano

colnames(barcelona_total)[1] <-"temporada"
colnames(barcelona_total)[2] <-"fecha"
colnames(barcelona_total)[3] <-"equipo_local"
colnames(barcelona_total)[4] <-"equipo_visitante"
colnames(barcelona_total)[5] <-"goles_local_final"
colnames(barcelona_total)[6] <-"goles_visitante_final"
colnames(barcelona_total)[7] <-"resultado_final"
colnames(barcelona_total)[8] <-"goles_local_descanso"
colnames(barcelona_total)[9] <-"goles_visitante_descanso"
colnames(barcelona_total)[10] <-"resultado_descanso"


#a partir de aqui empezamos a modificar el df sobre el que trabajaremos:
#barcelona_total para quedarnos con los partidos con los que juega de local o visitante o para sacar las medias de los goles que marca cuando es local o visitante

barcelona_local <- barcelona_total %>%
  filter (equipo_local=="Barcelona")

barcelona_visit <- barcelona_total %>%
  filter (equipo_visitante=="Barcelona")

media_goles_local <- barcelona_local %>%
  summarise (media_goles_local = mean(goles_local_final))

media_goles_visit <- barcelona_visit %>%
  summarise (media_goles_visit = mean (goles_visitante_final))


#ahora modificaremos los datos de los jugadores 

jugadores_barcelona <- jugadores %>%
  filter (Squad == "Barcelona")  %>% #QUEREMOS SOLO LOS JUGADORES DEL BARÇA
  select(Player, Pos, Born, Age, Starts, Min, Gls, Ast) %>% #Y SOLO ESTAS COLUMNAS
  mutate (Pos =ifelse (Pos=="DF,MF", "DF", Pos)) %>% #CAMBIO POSICIÓN ERIC GARCIA
  mutate (Pos =ifelse (Pos=="MF,FW", "MF", Pos)) %>% #CAMBIO POSICIÓN GAVI
  mutate (Pos =ifelse (Pos=="FW,MF", "FW", Pos)) %>% #CAMBIO POSICIÓN FERRAN Y P.TORRE
  mutate (Pos =ifelse (Pos=="DF", "Defensa", Pos)) %>% #PASAMOS LENGUAJE + CÓMODO
  mutate (Pos =ifelse (Pos=="GK", "Portero", Pos)) %>%
  mutate (Pos =ifelse (Pos=="MF", "Centrocampista", Pos)) %>%
  mutate (Pos =ifelse (Pos=="FW", "Delantero", Pos))


#modificamos los nombres de las columnas
colnames(jugadores_barcelona)[1] <-"jugador"
colnames(jugadores_barcelona)[2] <-"posicion"
colnames(jugadores_barcelona)[3] <-"año"
colnames(jugadores_barcelona)[4] <- "edad"
colnames(jugadores_barcelona)[5] <-"titularidades"
colnames(jugadores_barcelona)[6] <-"minutos_jugados"
colnames(jugadores_barcelona)[7] <-"goles"
colnames(jugadores_barcelona)[8] <-"asistencias"



#ya podemos empezar a realizar los gráficos
#Primero iremos con los de los partidos

#gráfico 1
# COMO LOCAL Gráfico de barras para el resultado al descanso 
#A es Equipo Visitante, D es Empate y H es Equipo Local
ggplot(barcelona_local, aes(x = resultado_descanso, fill = resultado_descanso)) +
  geom_bar() +
  scale_fill_manual(values = c("H" = "blue", "D" = "yellow", "A" = "red")) +
  labs(
    title = "Resultados al descanso como Local",
    x = "Resultado",
    y = "Frecuencia",
    fill = "Resultado"
  ) +
  theme_minimal()

#gráfico 2
#COMO LOCAL Gráfico de barras para el resultado al final del partido
#A es Equipo Visitante, D es Empate y H es Equipo Local

ggplot(barcelona_local, aes(x = resultado_final, fill = resultado_final)) +
  geom_bar() +
  scale_fill_manual(values = c("H" = "blue", "D" = "yellow", "A" = "red")) +
  labs(
    title = "Resultados al final como Local",
    x = "Resultado",
    y = "Frecuencia",
    fill = "Resultado"
  ) +
  theme_minimal()

#gráfico 3
#COMO VISITANTE Gráfico de barras para el resultado al descanso
#A es Equipo Visitante, D es Empate y H es Equipo Local

ggplot(barcelona_visit, aes(x = resultado_descanso, fill = resultado_descanso)) +
  geom_bar() +
  scale_fill_manual(values = c("H" = "blue", "D" = "yellow", "A" = "red")) +
  labs(
    title = "Resultados al descanso del como Visitante",
    x = "Resultado",
    y = "Frecuencia",
    fill = "Resultado"
  ) +
  theme_minimal()

#gráfico 4
#COMO VISITANTE Gráfico de barras para el resultado al final
#A es Equipo Visitante, D es Empate y H es Equipo Local

ggplot(barcelona_visit, aes(x = resultado_final, fill = resultado_final)) +
  geom_bar() +
  scale_fill_manual(values = c("H" = "blue", "D" = "yellow", "A" = "red")) +
  labs(
    title = "Resultados al final como Visitante",
    x = "Resultado",
    y = "Frecuencia",
    fill = "Resultado"
  ) +
  theme_minimal()

#gráfico 5
#Comparación de resultados al descanso y al final cuando el equipo era LOCAL
#A es Equipo Visitante, D es Empate y H es Equipo Local

df_largo_local <- barcelona_local %>% 
  pivot_longer (cols = c(resultado_descanso, resultado_final), names_to = "momento", values_to = "resultado") 

#Calculamos proporciones 
proporciones_local <- df_largo_local %>% 
  count(momento, resultado) %>% 
  group_by(momento) %>% 
  mutate(Prop = n / sum(n)) 

#Combinamos el grafico 
ggplot(proporciones_local, aes(x = resultado, y = Prop, fill = momento)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("resultado_descanso" = "yellow", "resultado_final" = "red")) + 
  labs( title = "Comparación de resultados al descanso y al final como Local", x = "Resultado", y = "Proporción", fill = "Momento" ) + 
  theme_minimal()

#gráfico 6
#Comparación de resultados al descanso y al final cuando el equipo era VISITANTE
#A es Equipo Visitante, D es Empate y H es Equipo Local

df_largo_visit <- barcelona_visit %>% 
  pivot_longer (cols = c(resultado_descanso, resultado_final), names_to = "momento", values_to = "resultado") 

#Calculamos proporciones 
proporciones_visit <- df_largo_visit %>% 
  count(momento, resultado) %>% 
  group_by(momento) %>% 
  mutate(Prop = n / sum(n)) 

#Combinamos el grafico 
ggplot(proporciones_visit, aes(x = resultado, y = Prop, fill = momento)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("resultado_descanso" = "yellow", "resultado_final" = "red")) + 
  labs( title = "Comparación de resultados al descanso y al final como Visitante", x = "Resultado", y = "Proporción", fill = "Momento" ) + 
  theme_minimal()


#ahora cambiaremos al dataframe de los jugadores (jugadores_barcelona)

#GRAFICOS JUGADORES

#gráfico 7
#GOLES Y ASISTENCIAS POR POSICION

ggplot(jugadores_barcelona, aes(x = posicion)) +
  geom_bar(aes(y = goles, fill = "Goles"), stat= "identity", position = "dodge") +
  geom_bar(aes(y = asistencias, fill = "Asistencias"), stat= "identity", position = "dodge") +
  scale_fill_manual(values = c("Goles" = "blue", "Asistencias" = "green")) +
  labs(
    title = "Goles y Asistencias por Posición",
    x = "Posición en el Campo",
    y = "Cantidad",
    fill = "Rendimiento"
  ) +
  theme_minimal()

#gráfico 8
#EDAD VS RENDIMIENTO

jugadores_edad_rend <- jugadores_barcelona %>%
  mutate(edad = 2023 - año)

ggplot(jugadores_edad_rend, aes(x = edad, y = goles + asistencias)) +
  geom_point(aes(color = posicion), size = 4, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Edad vs Rendimiento",
    x = "Edad del Jugador",
    y = "Goles + Asistencias",
    color = "Posición"
  ) +
  theme_minimal()

#gráfico 9
#PARTICIPACIONES DE TITULARES POR POSICION

#Calculamos los porcentajes de titularidades por posición

titulares_pos <- jugadores_barcelona %>% 
  group_by(posicion) %>% 
  summarise(titularidades = sum(titularidades)) %>%
  mutate(porcentaje = 100 * titularidades / sum(titularidades))

#Gráfico "quesito" con porcentajes
ggplot(titulares_pos, aes(x = "", y = titularidades, fill = posicion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(
    title = "Distribución de Titularidades por Posición",
    x = "",
    y = ""
  ) +
  theme_void()

#gráfico 10
#RENDIMIENTO DE JUGADORES POR POSICION


grafico_mov <- jugadores_barcelona %>%
  group_by(año, posicion) %>%
  summarise(goles = sum(goles), asistencias = sum(asistencias))




graf <- ggplot(grafico_mov, aes(x = posicion, y = año, fill = goles + asistencias)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Rendimiento por posición según el año de nacimiento",
    x = "Posición",
    y = "Año",
    fill = "Goles + Asistencias"
  ) +
  theme_minimal() +
  
  transition_time(año)

animate(graf, duration=10, fps= 10) 

