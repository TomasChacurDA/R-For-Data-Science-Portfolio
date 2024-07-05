library(tidyverse)
data <- iris

view(data)
help(iris)

# Cuantas filas y columnas hay?
# 150 filas y 5 columnas (observaciones y variables)
glimpse(data)
nrow(data) # para solo ver filas
ncol(data) # para ver solo columnas

# Que representa cada fila?
# Medidas de sepalo y petalo de distintos tipos de especies de plantas.

# Que variables son Categoricas?
# La variable categorica es Species

# Que variables son Numericas?
# Todas menos Sepal Width, Sepal Lenght, Petal Width, Petal Lenght

# Estas ultimas son enteros o puntos flotantes?
# Puntos flotantes.

# Cuántas y cuáles son las especies de flores presentes en el dataset?
unique(data$Species)
# Son 3 especies, Setosa, Versicolor, Virginica

ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  xlab('Largo Cepalo [cm]') +
  ylab('Ancho Cepalo [cm]')

# ¿Cuántas variables necesitaron para hacer este gráfico?
#     Usamos 2 Variables
# ¿Qué representa cada uno de los puntos del gráfico?
#     Cada punto representa la observacion de una especie puntual y su ancho y largo ( segun la posicion del punto en el eje )
# ¿Qué información brinda este gráfico? ¿Se observa alguna relación clara entre ambas variables?
#     La informacion que brinda es el largo y ancho del cepalo de cada especie y planta 
#     No se puede ver clara la relacion ya que no hay distintivos para cada especie

# Para ver claras las relaciones agregamos color por especie al grafico

ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  xlab('Largo Cepalo [cm]') +
  ylab('Ancho Cepalo [cm]')

# Ahora podemos ver que mientras mas ancho sea el cepalo menor es el largo y esta relacion se ve en las 3 especies
# La especie mas ancha es Setosa
# La especie mas larga es Virginica
# La especie Versicolor esta en el medio

# Repetimos con los Anchos y Largos de Petalo

ggplot(data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  xlab('Largo Petalo [cm]') +
  ylab('Ancho Petalo [cm]')

# Podemos ver que mientras mas largo sea el Petalo, mas ancho es.
# Podemos deducir que mientras mas largo sea el Sepalo mas fino es, pero el Petalo es mas ancho.
# La especie Setosa es la que tiene menor ancho en el Petalo
# Versicolor Sigue en el medio
# Virginica es la que tiene Petalos mas Anchos y Largos


################################

# Análisis del dataset lifestyle

# ¿Cuántas observaciones tiene este dataset?
nrow(data)
# Tiene 44 Filas ( Observaciones )

# ¿Cuántas variables?
ncol(data)
# Tiene 14 Columnas ( Variables )

# ¿Qué representa cada fila? Es decir, ¿cuáles son las unidades de este dataset?
# Las unidades del Dataset son caracteristicas de trabajo, polucion y demas parametros que conforman la vida en una ciudad especifica.

# ¿Qué variables son categóricas?
# Pais, Idioma, Rank, Continente

# ¿Qué variables son numéricas?
# Rank, Horas_sol, costo_agua, nivel_obesidad, ....

# Para las variables categóricas: explicar qué representa la variable y cuáles son las posibles categorías dentro de la misma.

# La variable Pais representa el Pais al cual pertenece cada Ciudad y sus posibles categorias son todos los paises del mundo.
# La variable Rank representa la posicion en el rank mundial de Ciudades mas Felices, y las posibles categorias son su posicion (mas cerca de 1 mejor)
# Idioma representa el idioma que se habla en esa determinada ciudad y sus posibles categorias son todos los idiomas.
# La variable Continente representa los Continentes a los que pertenece cada Pais y Ciudad, las posibles categorias serian asia, oceania, america, etc.


# Para las variables numéricas: ¿son continuas o discretas? ¿Qué variables tienen formato float?

# La variable Horas Sol,  es una variable Discreta y tiene formato Float
# La variable Costo Agua, es una variable Continua y tiene formato Float
# Nivel de Obesidad, es una variable Continua y tiene formato Float
# Indice Polucion es una variable Continua y tiene formato Float
# Actividades al exterior es una variable Discreta y tirnr formato Int ( Aunque es Float el tipo de dato esta ingresado como INT)
# Costo Gym es una variable Continua ya que puede tomar cualquier valor en un rango infinito y es de tipo Float.


# ¿Qué variables se podrían usar para hacer un scatter plot? Elijan dos variables cuyo gráfico podría brindarles información interesante. 
# ¿Cómo se imaginan que va a resultar este gráfico?

ggplot(data, aes(x = costo_gimnasio, y = nivel_obesidad)) +
  geom_point() +
  xlab('Costo Gym') +
  ylab('Nivel Obesidad')


#Realizar el gráfico que pensaron en el anterior ¿Qué resultados obtuvieron? ¿Resultó como se habían imaginado? ¿Por qué?
      # Nos imaginamos que mientras mas caro menos nivel de obesidad iba a haber pero,
      # la realidad es que el nivel de obesidad se mantuvo en 50$ para arriba aunque hay menos casos.
  #¿Cómo mejorarían el gráfico?
      # Buscaria alguna variable categorica para ver alguna otra relacion.
  #¿Es necesario ajustar los límites de los ejes?
      # En este caso no.
  #¿Hay algún punto que sobresalga o que les llame la atención?
      # No.