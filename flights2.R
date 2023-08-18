#Script para realizar un analisis de tiempo de vuelo de aerolineas en 2013

#librerias

library("pacman")
p_load("dplyr", 
       "ggplot2", 
       "vroom")

#Lectura de datos

flights <- vroom(file = "https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/flights.txt")

#grafico de puntos que permita contestar cual es la aerolinea0
#con más tiempo promedio de vuelo
#y cual es la aerolinea con menos tiempo promedio de vuelo

###¿Qué aerolínea tiene el tiempo de viaje más largo?¿es la misma todo el año?
###¿Qué aerolínea tiene el tiempo de viaje más corto?¿es la misma todo el año?


#Data handling

#reduccion de data, agrupacion por aerolinea, mes y tiempo de vuelo

flights_1 <- flights %>% 
  select(carrier, month, air_time) %>%             #seleccionamos aerolinea, meses y tiempo de vuelo
  group_by(carrier, month) %>%                    #los agrupamos
  summarise(Count = n())                          #sumamos para tener tiempo de vuelo por mes

#graficando tiemmpo de vuelo por mes para cada aerolinea 

flights_1.p <- ggplot(flights_1,             #asignamos los datos que vamos a graficar
                       aes(x = month,   #mi eje x sera 
                           y =  Count,       #mi eje y sera
                           colour = carrier)) +  #colorearemos segun el tiempo dedicado
  geom_point(size= 3, 
             alpha = 0.5) + 
  geom_line(size= 1, 
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  scale_y_continuous(breaks = seq(1, 6000, by = 250))

#vis

flights_1.p

#Poniendo wapo el plot

flights_2.p <- flights_1.p +
  labs(title="Tiempo de vuelo por aerolínea", 
       subtitle="por mes durante el 2013") + 
  xlab("Mes") +
  ylab("Tiempo de vuelo en minutos") +
  theme_minimal()

#vis

flights_2.p
