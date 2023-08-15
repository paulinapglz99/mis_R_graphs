#Script para analizar nefrectomia parcial

#librerias

library("pacman")
p_load("vroom",
       "dplyr",
       "ggplot2")

#cargar datos

nefrectomia <- vroom(file = "NEFRECTOMIA_PARCIAL.csv")

#data handling

#groupby y summarise para tener los datos. Tabla de conteo de TIPO_DE_PROCEDIMIENTO

tabla_conteo <- nefrectomia %>% 
  count(TIPO_DE_PROCEDIMIENTO)

## Porcentaje de personas que tuvieron procedimiento robot o abierto. 


rate_procedimiento <- ggplot(tabla_conteo,aes(x="",y=n, fill=TIPO_DE_PROCEDIMIENTO)) +
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=n),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y") +
  scale_fill_manual(values=c("salmon","steelblue"))+
  theme_minimal()+
  labs(title="Porcentaje de procedimientos")

#Vis

rate_procedimiento

#abierta y robot vs tiempos quirurgicos

robos_tiempos_quirurgicos <- nefrectomia %>% 
  ggplot(aes(x = TIPO_DE_PROCEDIMIENTO,
                         y = TIEMPO_QUIRURGICO, 
                        col = LATERALIDAD)) +
  geom_point( size = 3) + 
  scale_y_continuous(breaks = seq(from = 0, to = 300, by = 25)) +
  theme_minimal() +
  labs(x="Tipo de Procedimiento", y="Tiempo quirurgico")

#Vis

robos_tiempos_quirurgicos

#abierta y robot vs dias de EIH

robos_dias_EIH <- ggplot(nefrectomia) + 
  geom_density(aes(x = DIASDE_EIH, fill = TIPO_DE_PROCEDIMIENTO), position = 'stack') + 
  xlab("Días de EIH") + 
  ylab("Tipo de procedimiento") + 
  theme_minimal()

#Vis

robos_dias_EIH

#Guardar los plots