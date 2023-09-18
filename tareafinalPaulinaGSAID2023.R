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
         'vroom',       #lectura de datos
        'stringr',     #data handling
       'viridis',     #colores 
        'cowplot')    #para el plot_grid

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
  
good.shapes <- c(1:24, 32:127)

#Hago el conteo para MX

conteo_variante_month_MX <- gsaid2023 %>% 
  filter(country == 'Mexico') %>% 
  group_by(month, pango_lineage) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n()) %>%  #cuenta 
  arrange(by = month, count) #organice por pais, mes y conteo
    
#Hago el conteo para USA. Aqui tenemos tantos datos que vamos a poner un threshold de >100

conteo_variante_month_USA <- gsaid2023 %>% 
  filter(country == 'USA') %>% 
  group_by(month, pango_lineage) %>%     #los agrupamos y sumamos por pais y conteo
  summarise(count = n()) %>%  #cuenta 
  arrange(by = month, count) %>% #organice por pais, mes y conteo
filter(count > 107)

#antes de plotear, quiero hacer un conteo rapido de cuantos de variables hay

lista_pangos <- gsaid2023 %>% 
  ungroup() %>% 
  select(pango_lineage) %>% 
  arrange(pango_lineage) %>% 
  distinct()

#hay 486 , quiero asignar un tipo de glifo a cada tipo de variable para 


#ploteando para mexico

conteo_variante_month_mexico.p <- conteo_variante_month_MX %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count, #mi eje y sera
           shape = pango_lineage,
             colour = pango_lineage)) +   #colorear las lineas segun la variante
  scale_shape_manual(values=good.shapes) +
   geom_line(show.legend = FALSE) +  #grafico de linea
  geom_point(size = 5) +  #grafico de punto para dar estetica 
  geom_label(aes(label=ifelse(count == max(count),
                              as.character(count), NA)), 
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + #para que aparezcan todos los meses
  labs(title="Numero de secuenciaciones de SARSCoV2",           #titulos
       subtitle='por mes durante 2023 para MX segun GSAID') + 
  xlab("Mes") +
  ylab("Numero de secuenciaciones") +
  theme(legend.title = element_text('Variantes pango_lineage')) +
  theme_bw()

#vis 

conteo_variante_month_mexico.p

#ploteando para USA

conteo_variante_month_USA.p <- conteo_variante_month_USA %>% 
  ggplot(aes(x = month,   #mi eje x sera 
             y =  count, #mi eje y sera
             shape = pango_lineage,
             colour = pango_lineage)) +   #colorear las lineas segun la variante
  geom_line(show.legend = FALSE) +
  scale_shape_manual(values=good.shapes) +
  geom_point(size = 5) +
  geom_label(aes(label=ifelse(count == max(count),
                              as.character(count), NA)), 
             show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) + #para que aparezcan todos los meses
  labs(title="Numero de secuenciaciones de SARSCoV2", 
       subtitle='por mes durante 2023 para USA segun GSAID') + 
  xlab("Mes") +
  ylab("Numero de secuenciaciones") +
  theme_bw()

#vis

conteo_variante_month_USA.p

#juntar graficos

plot_grid(conteo_variante_month_mexico.p, conteo_variante_month_USA.p, 
            labels = c('A', 'B'),
            ncol = 1)
