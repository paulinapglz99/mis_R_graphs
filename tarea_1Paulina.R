
##Tarea 1
##Exploracion de base de datos.
#Alejandra Paulina Perez Gonzalez

library(pacman)
p_load('dplyr', 
       'vroom')

iris_esp <-  read.table(file = 'https://raw.githubusercontent.com/INMEGEN/CienciaDeDatosConR/master/data/iris_esp.csv',
                      sep=',',header= TRUE, stringsAsFactors = TRUE)

##Los titulos de las columnas no son practicos, tienen acentos y mayusculas y minusculas

#renombrarlos

colnames(iris_esp) <- c("sepalo_longitud", "sepalo_ancho", 
                        "petalo_largo", 'petalo_ancho', 
                        "especie")

#La otra opcion es ponerlos en minusculas y quitar los acentos

iris_esp <- rename_with(iris_esp, tolower)

colnames(iris_esp) <- colnames(iris_esp) %>%  
  chartr(old = 'áéíóúñ',
         new = 'aeioun')

##ver resumen de los datos

summary(iris_esp) 

#Boxplot de los datos

boxplot(iris_esp)

##Exploración mundiales femeninos

mundiales <- readxl::read_xlsx(path = 'mundiales_femenil.xlsx')  

