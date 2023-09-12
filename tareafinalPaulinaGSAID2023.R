#Tarea evaluativa curso RDataScience 2023
#Alejandra Paulina Perez Gonzalez - paulinapglz.99@gmail.com


#Usar datos de gsaid_2023.txt

#Mision: 
#Analisis de variantes genomicas de SARS-CoV2. Las preguntas son:

#1)Que pais ha hecho mas secuenciacion de SARS en 2023?
#2) En que mes se ha hecho mas secuenciacion? 
#3) Cual(es) es la variante(s) dominante(s) en cada mes por pais?
#4) Realizar reporte en RMarkdown, chunks por analisis

setwd("~/tareas_RDataScience")

#Librerias

library(pacman)
p_load( 'tidyverse',   #data handling
        'ggplot2',     #plotting
        'vroom',       #lectura de datos
        'stringr')    #data handling


#Lectura de datos

gsaid2023 <- vroom(file = 'https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/ProyectoIntegrador/datos_gisaid_2023.txt')

#En la exploracion de dataset, vemos que la columna de fecha es una sola.
# La columna de "variante" tambien es una sola, por lo que hay que splitear las 
#columnas para obterner la informacion necesaria

#Nota: ECDC utilises three categories of variant classification to communicate 
#increasing levels of concern about a new or emerging SARS-CoV-2 variant:
#variant under monitoring (VUM), variant of interest (VOI) and variant of concern (VOC).

gsaid2023 <- gsaid2023 %>% 
  mutate(anio = year(collection_date), 
         month = month(collection_date)) %>% #usar lubridate para crear la columna de anio y mes
         select(-collection_date) #eliminar col repetida

gsaid2023 <- gsaid2023 %>% 
  separate_wider_delim(cols = variant,
                       delim = " first dete",  #el dataset tiene un error, asi que el delimitador debe de ser este y despues eliminar el string restante
                       names = c("type_of_variant", 
                                 "first_detected_in"))
#eliminar el remanente

gsaid2023$first_detected_in <- str_replace_all(string = gsaid2023$first_detected_in,
                  pattern = "cted in ",
                  replace = "")

#1)Que pais ha hecho mas secuenciacion de SARS en 2023?

conteo_seq <- gsaid2023 %>% 
  group_by(country) %>%     #los agrupamos y sumamos
  summarise(count = n())

#grafica rapida para analizar

conteo_seq.p <- conteo_seq %>% 
  ggplot(aes(x = country,   #mi eje x sera 
             y =  count)) +       #mi eje y sera
  geom_col()

#vis del plot

conteo_seq.p

#el pais que ha hecho mas secuenciacion de SARS-CoV2 en 2023 es
#USA

#2) En que mes se ha hecho mas secuenciacion? En enero jej

conteo_seq_month <- gsaid2023 %>% 
  group_by(month) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n())

#ploteando

conteo_seq_month.p <- conteo_seq_month %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count)) +       #mi eje y sera
  geom_col()


#3) Cual(es) es la variante(s) dominante(s) en cada mes por pais? este si va a estar mas perru

conteo_variante_month_country <- gsaid2023 %>% 
  group_by(month, type_of_variant, country) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n())

#ploteando

conteo_variante_month_country.p <- conteo_variante_month_country %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count, 
             fill = type_of_variant)) +       #mi eje y sera
  geom_col()
