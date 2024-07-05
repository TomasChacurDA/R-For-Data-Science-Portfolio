library(tidyverse)
library(stats)
df <- read_csv("C:/Users/Tomas/Desktop/Lic Ciencia de Datos/Introduccion a Ciencia de Datos/Clases/Clase 3 Descripcion/arbolado_comuna14.csv")
view(df)
# Filas == 22417
# Columnas == 18

length(df)
# Nos da la cantidad de Columnas que tiene el DF

nrow(df)
# Nos da la cantidad de Registros ( Filas ) del DF

# Creamos una tabla con las siguientes 3 especies
tres_especies <- df %>% 
  filter(nombre_cientifico %in% c('Tilia x moltkei', 'Fraxinus excelsior', 'Melia azedarach'))

# ¿Cuántos árboles de cada especie hay?

count(tres_especies, nombre_cientifico)

# Qué especie de árboles (entre las tres elegidas) es la más alta?”

ggplot(tres_especies) +
  geom_jitter(mapping = aes(x = nombre_cientifico, y = altura_arbol, color = nombre_cientifico), 
              alpha = 0.2, width = 0.4, height = 0, na.rm = TRUE) 

# El width es apropiado ya que nos ayuda a dispersar el ancho del eje x, y los elementos de cada clase se ven separados

# ¿Pueden a partir de este gráfico ordenar las especies por altura?
# ¿Cuáles serían sus argumentos para cada orden?
  # Podemos tener una idea general pero no concreta, ya que los distintos tipos de arbol
  # tienen vaores parecidos en algunas partes del eje Y
  # creo que meila azearach es la mas alta porque la zona de puntos mas concentrada esta mas arriba
  # que las otras dos. la tilia x nltkei seria la segunda, ya que (aunque la zona concentrada superior
  # parece semejante a la meilia) se dispersa antes que la primera y la ultima seria fraxinus excelsior
  # porque la zona concentrada se encuentra por debajo de las otras dos.

df_resumen <- group_by(tres_especies, nombre_cientifico) %>%
  summarise(altura.promedio = mean(altura_arbol,na.rm = TRUE))


ggplot(df_resumen) +
  geom_hline(data = df_resumen, aes(yintercept = altura.promedio, color = nombre_cientifico))

# Le agregamos el Hline al grafico de dispersion

ggplot(tres_especies) +
  geom_jitter(mapping = aes(x = nombre_cientifico, y = altura_arbol), height = 0, width = 0.4) +
  geom_hline(data = df_resumen, aes(yintercept = altura.promedio, color = nombre_cientifico), na.rm = T)

# ¿Cómo podrían ordenarse por alturas las especies?
  # Melia 
  # Tilia
  # Fraxinus

altura_max <- group_by(tres_especies, nombre_cientifico) %>%
  summarise(altura.maxima = max(altura_arbol,na.rm = TRUE))

# Definir a que altura maxima puede llegar cada especie
ggplot(tres_especies) +
  geom_jitter(mapping = aes(x = nombre_cientifico, y = altura_arbol), height = 0, width = 0.4) +
  geom_hline(data = df_resumen, aes(yintercept = altura.promedio, color = nombre_cientifico)) + # Promedios de altura
  geom_segment(data=altura_max, aes(x = 0, y=altura.maxima, yend = altura.maxima, xend=3.5, color = nombre_cientifico)) # Altura maxima

# Si alguien por error agrega un 0 de mas en la especie Fraxinus, como cambiaria el resultado?

  # Obtenemos el valor de altura máxima de Fraxinus
max_fraxinus <- altura_max[altura_max$nombre_cientifico=='Fraxinus excelsior',]$altura.maxima
  # Creamos una nueva tabla donde le sacamos un cero a ese valor)
nueva_tabla <- mutate(df, altura_arbol = if_else(altura_arbol == max_fraxinus & nombre_cientifico == 'Fraxinus excelsior', max_fraxinus/10, altura_arbol) )


quantile(df$altura_arbol, probs = 0.95, na.rm=TRUE)
# Que informacion nos dan estos numeros?

  # El cuantil 0.95 de un conjunto de datos de alturas de árboles, 
  # que en este caso es de 25 metros, indica que el 95% de las alturas de los árboles en el conjunto de datos son iguales o inferiores a 25 metros. 
  # En otras palabras, si ordenaras todas las alturas de los árboles de menor a mayor, 
  # el 95% de las alturas estarían por debajo de 25 metros y solo el 5% restante estaría por encima de ese valor. 
  # Esto puede ser útil para comprender la distribución y la variabilidad de las alturas de los árboles en el conjunto de datos.
quantile(df$altura_arbol, probs = c(0.05, 0.95), na.rm=TRUE)
quantile(df$altura_arbol, probs = c(0.05, 0.5, 0.95), na.rm=TRUE)



# Calcular el máximo de cada especie y los cuantiles 0.05 y 0.95 y agregar todo en un dataframe. 

altura_max2 <- group_by(tres_especies, nombre_cientifico) %>%
  summarise(altura.maxima = quantile(nueva_tabla$altura_arbol, probs = c(0.05, 0.95), na.rm=TRUE))
view(nueva_tabla)


quantiles_especies <- df %>% group_by(nombre_cientifico) %>% summarise(min=min(altura_arbol, na.rm=T), primer=quantile(altura_arbol, 0.25, na.rm=T),
              median=median(altura_arbol, na.rm=T), tercer=quantile(altura_arbol, 0.75, na.rm=T),
              max=max(altura_arbol, na.rm=T))

quantiles_tres_especies <- tres_especies %>% group_by(nombre_cientifico) %>% summarise(min=min(altura_arbol, na.rm=T), primer=quantile(altura_arbol, 0.25, na.rm=T),
                                                                       median=median(altura_arbol, na.rm=T), tercer=quantile(altura_arbol, 0.75, na.rm=T),
                                                                       max=max(altura_arbol, na.rm=T))

ggplot(data = tres_especies, aes(x = nombre_cientifico, y = altura_arbol),height = 0, width = 0.4) +
  geom_boxplot(fill = "skyblue")

ggplot(data = tres_especies, aes(x = nombre_cientifico, y = altura_arbol),color = nombre_cientifico, na.rm=T) +
  geom_jitter(height = 0, width = 0.4,color = "yellow",na.rm=T)+
  geom_boxplot(fill = "skyblue",na.rm=T) 


ggplot(data = tres_especies, aes(x = nombre_cientifico, y = altura_arbol),color = nombre_cientifico, na.rm=T) +
  geom_boxplot(fill = "green",na.rm=T) +
  geom_jitter(height = 0, width = 0.4,color = "purple",na.rm=T)

