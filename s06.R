########## Cargar librerias ##########
library("ggplot2")
library("dplyr")
library("ggsci")
library("cowplot")
library("scales")



########## Codigo ##########

# Leer datos
sni.df <- read.table(file = "resumen_sni_por_sexo_nivel_anio.csv",
                     header = T,
                     sep = ",",
                     stringsAsFactors = F)


# graficos
# crear graficos de lineas base
lineas_1.p <- sni.df %>% 
  filter( nivel_de_investigador == "Candidato") %>% 
  ggplot( mapping = aes(x = anio,
                        y = miembros,
                        color = titulo
                        )) + 
    geom_line()

# visualizamos
lineas_1.p


# agregar capa de puntos, finamos escala Y de 0 a 11 mil
lineas_2.p <- lineas_1.p +
  geom_point() +
  scale_y_continuous(limits = c(0, 11000))
# c( 0,max (en objeto o directo) )

# visualizamos
lineas_2.p


# uso de ggsci (paleta d3)
lineas_3.p <- lineas_2.p +
  scale_color_d3() +
  ggtitle(paste0("Crecimiento del SNI \nNivel  ", "Candidato"))

# visualizamos
lineas_3.p


# agregar tema
lineas_4.p <- lineas_3.p + 
  theme_bw() +
  theme( axis.title = element_blank() )

# visualizamos
lineas_4.p


# crear graficos de barra base
barras_1.p <- sni.df %>% 
  filter(nivel_de_investigador == "Candidato") %>% 
  ggplot(mapping = aes(fill = titulo,
                       y = proporcion,
                       x = anio)) +
  geom_bar(position = "stack",
           stat = "identity") # stack lo apila

# visualizamos
barras_1.p


# convertir eje Y a porcentajes
barras_2.p <- barras_1.p +
  scale_y_continuous(breaks = seq(from = 0, 
                                  to = 1,
                                  by = 0.1),
                     label = percent)

# visualizamos
barras_2.p


# modificar escala x para mostrar years
barras_3.p <- barras_2.p +
  scale_x_continuous(breaks = 2015:2020)

# visualizamos
barras_3.p


# lo volvemos visualmente atractivo
barras_4.p <- barras_3.p +
  scale_fill_d3() +
  ggtitle(paste0("Crecimiento del SNI \nNivel ", "Candidato"))

# visualizamos
barras_4.p


# agregar tema
barras_5.p <- barras_4.p + 
  theme_bw() +
  theme( axis.title = element_blank() )

# visualizamos
barras_5.p


# Ahora si metemos ambos plots finales en un grid vertical ----
plot_grid( lineas_4.p, barras_5.p, ncol = 1 )


# funcion graficadora
graficadora.f <- function(la_data, el_nivel) {
  
  # generamos el plot de lineas  
  las_lineas.p <- la_data %>% filter( nivel_de_investigador == el_nivel ) %>%
    ggplot( mapping = aes( x = anio, y = miembros, color = titulo ) ) +
    geom_line() +
    geom_point() +
    scale_y_continuous( limits = c(0, 11000) ) +
    scale_color_d3() +
    ggtitle( paste0("Crecimiento del SNI \nNivel ", el_nivel) ) +
    theme_bw() +
    theme( axis.title.x = element_blank() )
  
  # generamos el plot de barras  
  las_barras.p <- la_data %>% filter( nivel_de_investigador == el_nivel ) %>%
    ggplot( aes( fill = titulo, y = proporcion, x = anio ) ) + 
    geom_bar( position = "stack", stat = "identity" ) +
    scale_y_continuous( breaks = seq(from = 0, to = 1, by = 0.1),
                        label = percent ) +
    scale_x_continuous( breaks = 2015:2020) +
    scale_fill_d3() +
    ggtitle( paste0("Crecimiento del SNI \nNivel ", el_nivel) ) +
    theme_bw() +
    theme( axis.title.x = element_blank() )
  
  #Juntamos los plots en una rejilla de columna
  plot_grid( las_lineas.p, las_barras.p, ncol = 1)
}

# probamos la funcion
graficadora.f(la_data = sni.df,
              el_nivel = "Candidato")


# Generamos los 4 plots, 1 por cada nivel para comparacion
# Usemosla para generar graficos para cada nivel
candidato.p <- graficadora.f( la_data = sni.df, el_nivel = "Candidato" )

uno.p <- graficadora.f( la_data = sni.df, el_nivel = "1" )

dos.p <- graficadora.f( la_data = sni.df, el_nivel = "2" )

tres.p <- graficadora.f( la_data = sni.df, el_nivel = "3" )

# juntar plots en gradilla
final.p <- plot_grid( candidato.p, uno.p, dos.p, tres.p, nrow = 1)

# visualizar
final.p


