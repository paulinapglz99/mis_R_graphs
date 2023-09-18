#Tarea 4
#Alejandra Paulina Perez Gonzalez


#librerias a usar

library('pacman')
p_load('dplyr',
       'tidyr',
       'ggplot2', 
       'vroom',
       'cowplot')

#primer plot 

pa <- iris %>% 
  ggplot(mapping = 
           aes(x =Petal.Length,
               y = Petal.Width,
               fill = Species)) +
  geom_point(shape = 23)

#boxplot

pb <- iris %>% 
  ggplot(mapping = 
           aes(x =Petal.Length,
               y = Petal.Width,
               fill = Species)) +
  geom_boxplot()
  
#Otro plot

pc <- iris %>% 
  ggplot(mapping = 
           aes(x = Species,
               y = Petal.Width,
               fill = Species)) +
  geom_boxplot()

#grid 

plot_grid(pa, pb, pc)


#queremos hacer que el panel de arriba sea solo la dispersion

plot_grid(pa,NULL, pb, pc)

#queremos hacer que el panel de arriba sean los boxplots

plot_grid(pa,NULL, pb, pc, 
          labels = c('A', '', 'B', 'C'))

#queremos hacer que se distribuya pa en toda la fila

plot_grid(
  plotlist = list(
    pa, 
    plot_grid (
      plotlist = list(pb, pc), 
    labels = c('B', 'C'),
     ncol = 2
    )
  ),
  labels = c('A', ''),
  nrow = 2, ncol = 1
)

##TAREA

#Persona disconforme pide que Coincidan los ejes del diagrama
#de dispersión y elboxplot inferior


