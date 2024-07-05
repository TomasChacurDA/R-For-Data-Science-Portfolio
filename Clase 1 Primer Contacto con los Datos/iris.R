library(tidyverse) # para cargar el paquete en la sesión actual de R

data <- iris # para cargar el dataset iris en la variable data

view(data) # para ver en formato "tabla" el dataset 

help(iris) # para acceder a documentación del dataset

unique(data$Species) # para conocer los elementos únicos de la variable Species de data
unique(data["Species"]) # otra forma para conocer los elementos únicos de la variable Species de data

ggplot(data, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point() + 
  xlab("Largo del Sépalo [cm]") + 
  ylab("Ancho del Sépalo [cm]")

ggplot(data, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point() + 
  xlab("Largo del Sépalo [cm]") + 
  ylab("Ancho del Sépalo [cm]")
