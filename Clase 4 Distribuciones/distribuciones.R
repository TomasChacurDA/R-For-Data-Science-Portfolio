# El código en este archivo está pensando como guía para el trabajo de laboratorio. 
# El código tal cuál está escrito no funciona, tienen que completar donde aparecen signos de interrogación.

library(tidyverse)

# Leemos los datos
df <- read_csv('C:/Users/Tomas/Desktop/Lic Ciencia de Datos/Introduccion a Ciencia de Datos/Clases/Clase 4 Distribuciones/insurance.csv')

# Punto 5 de la guía
df %>% 
  # Filtrar solo fumadores varones
  filter(smoker=='yes', sex == 'male') %>% # Al filtrar obtenemos 149 Observaciones y conserva sus 7 Variables.
  ggplot(aes(x=age)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_boxplot(aes(y = 3.0), fill='blue') + 
  geom_dotplot(binwidth=1, 
               fill='lightblue', color='transparent',
               dotsize=0.5) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x='Edad [Años]',
       title='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

# Punto 7 
  # Mismo Grafico, sin filtrados
    # Se amontonan los puntos del grafico ya que estamos tomando el DF completo.
    # En este caso no sirve el grafico
    # La limitacion del dotplot es que cuando hay demasiados datos se empiezan a superponer y no podemos analizar bien la grafica.
    # No usen este tipo de grafico cuando los datos son numerosos
df %>% 
  ggplot(aes(x=age)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_boxplot(aes(y = 3.0), fill='blue') + 
  geom_dotplot(binwidth=0.8, 
               fill='lightblue', color='transparent',
               dotsize=0.7) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x='Edad [Años]',
       title='Costo del seguro de salud',
       subtitle='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

# Punto 8
df %>% 
  # Filtrar solo fumadores varones
  filter(smoker=='yes', sex == 'male') %>% # Al filtrar obtenemos 149 Observaciones y conserva sus 7 Variables.
  ggplot(aes(x=age)) +
  geom_jitter(aes(y=2.0), width=0) +
  geom_boxplot(aes(y = 3.0), fill='blue') + 
  geom_dotplot(binwidth=1, 
               fill='lightblue', color='transparent',
               dotsize=0.5) +
  # Para sacar las etiquetas del eje y, que no indican nada
  scale_y_continuous(name=NULL, labels=NULL) +
  labs(x='Edad [Años]',
       title='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))

# Punto 9
  # Los bins ( 'bins = 10', por ejemplo ) indican la cantidad de barras que queremos que haya en el grafico de histograma
  # Los bindwidth indican el ancho que toma la barra segun la cantidad de edades que queramos que haya en cada barra

df %>% 
  filter(smoker=='yes', sex == 'male') %>% 
  ggplot(aes(x=age)) +
  geom_jitter(aes(y=10.0), width=0) +
  geom_boxplot(aes(y = 15.0), fill='salmon') + 
  geom_histogram(binwidth=1, color = 'transparent') + 
  labs(x='Edad [Años]',title='Varones fumadores de EEUU') +
  theme(axis.title = element_text(size=14))


# Punto 10.
  bw <- 5000
  df %>% 
  filter(smoker=='yes', sex=='male') %>%
  ggplot(aes(x=charges)) +
  # Grafica el histograma
  geom_histogram(binwidth=bw, 
                 fill='darkgreen', color='transparent') +
  stat_bin(aes(y=..count.., label=..count..), binwidth=bw,
           geom='text', vjust=-.5) +
  labs(y='Cantidad de Personas', x='Costo del seguro [USD]')

# Punto 11
bw <- 500
  df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=smoker)) +
  # Completen la posición. Vean la diferencia entre 'stack' y 'identity'
  geom_histogram(binwidth=bw, color='transparent', position='identity',
                 alpha=0.5) +
  labs(y='Cant. Varones', x='Costo del seguro [USD]')
  
# Punto 12
  # ¿sirve este gráfico para comparar cómo difieren los gastos de seguro entre varones fumadores y no fumadores? 
    # Si podemos ver la diferencia, los varones No fumadores pagan menos y son mas que los Si fumadores
  
  #¿Cuál es el tamaño de cada uno de los grupos?
    # 

# Punto 13
  # ¿Cómo difiere este histograma del realizado en el punto 11?
    # En uno se representa la cantidad de Varones respecto al Costo de Seguro
    # El grafico del punto 13 representa la Densidad de la cantidad de Varone respecto al Costo del Seguro

  
bw <- 2000
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=smoker)) +
  # Completen el mapeo usando la densidad ('density').
  #Vean cómo se referencian las variables calculadas por el geom en el punto 10
  geom_histogram(aes(y=..density..), position='identity', binwidth=bw, 
                 color='transparent', alpha=0.5) +
  labs(y='Densidad', x='Costo del seguro [USD]')

"""

El principal beneficio de conocer y utilizar la densidad en este caso es que te proporciona
una mejor comprensión de la distribución de los datos. 
Comparado con un histograma normal(Punto 11) que muestra la frecuencia de los datos en 
cada intervalo, el histograma de densidad (PUNTO 13) muestra la densidad de probabilidad 
en lugar de la frecuencia. Esto puede ser útil en situaciones donde las muestras tienen 
diferentes tamaños o cuando quieres comparar distribuciones con diferentes unidades o 
escalas.
En tu primer código, estás utilizando un histograma estándar para mostrar la cantidad de 
varones (frecuencia) en diferentes rangos de costos de seguro. 
Esto es útil para tener una idea general de la distribución de los datos.
En el segundo código, estás utilizando un histograma de densidad, que normaliza los 
datos de modo que el área total debajo de la curva sea igual a 1. Esto te da una mejor
idea de la forma de la distribución sin verse afectada por el número total de observaciones.
Esto puede ser útil cuando quieres comparar distribuciones entre grupos con diferentes 
tamaños de muestra.

Entonces, es recomendable utilizar un histograma de densidad cuando:

Quieres comparar distribuciones con diferentes tamaños de muestra.
Tienes diferentes unidades o escalas en tus datos.
Deseas visualizar la forma de la distribución sin verse afectado por el número total de
observaciones.
En resumen, el uso de la densidad en lugar de la frecuencia en un histograma puede 
proporcionar una comprensión más completa y equitativa de la distribución de tus datos 
en ciertos escenarios.

"""

# Punto 14 y 15
  # Lo que nos lleva a elegir un ancho de banda sobre otros es que estamos buscando el valor que mejor describe los datos.
bw <- 2900
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=smoker)) +
  # Completen el mapeo usando la densidad ('density').
  # Vean cómo se referencian las variables calculadas por el geom en el punto 10
  geom_histogram(aes(y=..density..), position='identity', binwidth = bw, 
                 color='transparent', alpha=0.5) +
  labs(y='Densidad', x='Costo del seguro [USD]') + geom_density()

# Punto 16 usando Histograma
bw <- 2000
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=region)) +
  # Después de probar, cambiar esta línea para usar freqpoly
  geom_histogram(aes(y=..density..), position='identity', binwidth=bw, 
                 color='transparent', alpha=0.3) +
  labs(y='Cuentas', x='Costo del seguro [USD]')

# Punto 16 Usando Freqpoly

bw <- 2000
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=region)) +
  # Después de probar, cambiar esta línea para usar freqpoly
  geom_freqpoly(aes(y=..density.., color = region), position='identity', binwidth=bw, alpha=0.5) +
  labs(y='Cuentas', x='Costo del seguro [USD]')

# Punto 17; Violin
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=region, y=charges)) +
  geom_violin() +
  labs(x='Region', y='Costo del seguro [USD]')

# Punto 18
  # Agregamos las marcas de los quantiles que necesitemos al grafico y color
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=region, y=charges)) +
  geom_violin(aes(color = region), draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(x='Region', y='Costo del seguro [USD]')



# Punto 19; ggridges
install.packages('ggridges')
library(ggridges)
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, fill=smoker, height = stat(density))) +
  geom_density_ridges(aes(y=region), alpha=0.5, stat='density') +
  labs(y='Region', x='Costo del seguro [USD]')


# Punto 20; geom_density2d
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, y=bmi, color=smoker)) +
  geom_density_2d() +
  scale_color_discrete(name='Fumador', labels=c('No', 'Sí')) +
  labs(y='Índice de masa corporal', x='Costo del seguro [USD]',
       title='Distribución de BMI y costos del seguro médico',
       subtitle='Varones de EEUU') +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 

# Punto 21 Prueba de diferentes Geometrias de libreria 'ggridges'
df %>% 
  filter(sex=='male') %>%
  ggplot(aes(x=charges, y=bmi, color=smoker)) +
  geom_bin_2d() +
  scale_color_discrete(name='Fumador', labels=c('No', 'Sí')) +
  labs(y='Índice de masa corporal', x='Costo del seguro [USD]',
       title='Distribución de BMI y costos del seguro médico',
       subtitle='Varones de EEUU') +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size=12),
        title = element_text(size=14),
        legend.title = element_text(size=12)) 
