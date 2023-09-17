#Tarea evaluativa curso RDataScience 2023
#Alejandra Paulina Perez Gonzalez - paulinapglz.99@gmail.com


#Usar datos de gsaid_2023.txt]

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
       'ggrepel',
         'vroom',       #lectura de datos
        'stringr')      #data handling
        

#Lectura de datos

gsaid2023 <- vroom(file = 'https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/ProyectoIntegrador/datos_gisaid_2023.txt')

#En la exploracion de dataset, vemos que la columna de fecha es una sola.
# La columna de "variante" tambien es una sola, por lo que hay que splitear las 
#columnas para obterner la informacion necesaria

gsaid2023 <- gsaid2023 %>% 
  mutate(anio = year(collection_date), 
         month = month(collection_date)) %>% #usar lubridate para crear la columna de anio y mes
         select(-collection_date) #eliminar col repetida

#Para hacer el tidy tenemos dos opciones, 

  ##Merequetengue con dplyr porque eldataset tiene un error en la columna
#variant, y se corta en la parte de la string " first dete"

#usando OPCION 1: separate_wider_delim


gsaid2023 <- gsaid2023 %>% 
  separate_wider_delim(cols = variant,
                       delim = " first dete",  #el delimitador debe de ser este y despues eliminar el string restante
                       names = c("type_of_variant", 
                                 "first_detected_in"))
#eliminar el remanente

gsaid2023$first_detected_in <- str_replace_all(string = gsaid2023$first_detected_in,
                  pattern = "cted in ",
                  replace = "")

#OPCION 2: usar strplit, tambien se debe eliminar el remanente con str_replace_all
 
strsplit(gsaid2023$variant, split = " first dete")

#respondiendo las preguntas

#1)Que pais ha hecho mas secuenciacion de SARS en 2023?

conteo_seq <- gsaid2023 %>% 
  group_by(country) %>%     #los agrupamos y sumamos
  summarise(count = n())

#grafica rapida para analizar

conteo_seq.p <- conteo_seq %>% 
  ggplot(aes(x = country,   #mi eje x sera 
             y =  count, 
             fill = country)) +       #mi eje y sera
  geom_col() + 
  geom_label(aes(x = country,
                 label = count)) +
  labs(title="Conteo de secuenciacion de SARSCoV2", 
       subtitle='durante 2023') + 
  xlab("Pais") +
  ylab("Secuenciaciones") +
  theme_bw()
  
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
  geom_point() + 
  geom_line() +
  geom_label(aes(label = count)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + #para que aparezcan todos los meses
  labs(title="Numero de secuenciaciones de SARSCoV2", 
       subtitle='por mes durante 2023 para USA y MX segun GSAID') + 
  xlab("Mes") +
  ylab("Numero de secuenciaciones") +
  theme_bw()

#vis

conteo_seq_month.p

#3) Cual(es) es la variante(s) dominante(s) en cada mes por pais?

#Hago el conteo para USA

conteo_variante_month_USA <- gsaid2023 %>% 
  filter(country == 'USA') %>% 
  group_by(month, pango_lineage) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n()) %>%  #cuenta 
  arrange(by = month, count) #organice por pais, mes y conteo
  
#Hago el conteo para MX

conteo_variante_month_MX <- gsaid2023 %>% 
  filter(country == 'Mexico') %>% 
  group_by(month, pango_lineage) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n()) %>%  #cuenta 
  arrange(by = month, count) #organice por pais, mes y conteo
    
#ploteando para mexico

conteo_variante_month_mexico.p <- conteo_variante_month_MX %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count, #mi eje y sera
             colour = pango_lineage)) +   #colorear las lineas segun la variante
  geom_line(show.legend = FALSE) +
  geom_label(aes(label=ifelse(pango_lineage == 'XBB.1.5',
                             as.character(count), NA)), 
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + #para que aparezcan todos los meses
  labs(title="Numero de secuenciaciones de SARSCoV2", 
       subtitle='por mes durante 2023 para MX segun GSAID') + 
  xlab("Mes") +
  ylab("Numero de secuenciaciones") +
  theme_bw()

#vis 

conteo_variante_month_mexico.p

#ploteando para USA

conteo_variante_month_USA %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count, #mi eje y sera
             colour = pango_lineage)) +   #colorear las lineas segun la variante
  geom_line(show.legend = FALSE) +
  geom_label(aes(label=ifelse(count == max(count),
                              as.character(count), NA)), 
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + #para que aparezcan todos los meses
  labs(title="Numero de secuenciaciones de SARSCoV2", 
       subtitle='por mes durante 2023 para USA segun GSAID') + 
  xlab("Mes") +
  ylab("Numero de secuenciaciones") +
  theme_bw()

#Nota: ECDC utilises three categories of variant classification to communicate 
#increasing levels of concern about a new or emerging SARS-CoV-2 variant:
#variant under monitoring (VUM), variant of interest (VOI) and variant of concern (VOC).
