library(tidyverse)

titanic_df <- read_csv("C:\\Users\\Tomas\\Desktop\\Lic Ciencia de Datos\\Introduccion a Ciencia de Datos\\Clases\\Clase 2 Manipulacion\\titanic.csv", locale = locale(decimal_mark = ","))

view(titanic_df)

spec(titanic_df)

glimpse(titanic_df)
#¿Qué diferencias hay entre los outputs del comando spec(...) y glimpse(...)?

  # Spec solo muestra que Columnas hay con el tipo de dato de cada una.
  # Glimpse muestra Cant. Total de Columnas, Entradas(Filas), tipo de dato y valores de las variables

#¿Qué información comparten?

  # Comparten Información de tipos de dato de las variables y nombres de las variables.

#¿Cuántas observaciones hay?

  # Hay 889 observaciones/entradas.

#¿Cuáles son las variables del dataset? ¿De qué tipos son?

  # Las Variables y Tipo de dato las vemos con "spec(titanic_df)"

#Muchas veces los tipos de las variables se cargan con valores por defecto como ‘chr’ (char). 
#Para las variables anteriores determinar si el tipo cargado es correcto o si consideran que debería ser otro.

  # sex = char
  # age = Es Float(double) deberia ser Int
  # sibilings_of_the_passenger = Es Float(double) deberia ser Int
  # parents_or_children = Es Float(double) deberia ser Int
  # fare = Esta bien, es precio del voleto y deberia ser double
  # Port_of_Embarkation = Está bien, es Char
  # class = Esta bien, es Char
  # who = Está bien, es Char
  # alone = Esta bien, es Logical, tiene solo 2 valores probables.
  # survived = Esta mal, debería ser Logico ya que hay solo 2 valores posibles.

#c. ¿Qué variables consideran que podrían utilizarse como categóricas?

  # survived
  # class
  # sex
  # Port_of_Embarkation
  # alone
  # who

#Seleccione todos los valores de la columna age (edad) ejecutando el comando
#titanic_df$age, van a ver que se los muestra todos en la consola. Si hacen
#min(titanic_df$age) están ejecutando la función mínimo sobre todos esos números.
#Háganlo y traten de explicar el resultado.

titanic_df$age
min(titanic_df$age, na.rm = TRUE)
# slice_min(titanic_df, age) esta buscando toda la fila completa de la persona con la edad minima 
slice_min(titanic_df, age)


# ¿Pueden decir ahora si la persona sobrevivió? 

slice_min(titanic_df, age) %>% relocate(survived)

# Si, sobrevivio

slice_max(titanic_df, age)
slice_max(titanic_df, age) %>% relocate(survived)

# También sobrevivio la persona mas grande que embarcó.

slice_max(titanic_df, age,n=5)

# es un recorte de las 5 personas mas grandes que embarcaron

slice_min(titanic_df, age,n=5)

# es un recorte de las 5 personas mas chicas que embarcaron

# a. ¿Cuál fue el boleto más barato (con fare menor)? ¿Y el más caro?.

slice_min(titanic_df, fare) #boleto mas barato 4.01
slice_max(titanic_df, fare) #boleto mas caro 512

# b. ¿Cuántas categorías de clase (class) había?.
#count(titanic_df,class)

length(unique(titanic_df$class)) #rta= 3 
#borra repetidos(unique)

#c. ¿Cuáles eran los puertos desde donde embarcaron (Port_of_Embarkation)?
unique(titanic_df$Port_of_Embarkation) #rta= "S", "C", "Q"

#¿Desde qué puerto embarcaron las dos personas más jóvenes que estaban en el barco?.
arrange(titanic_df,age) # Usando comando arrange
slice_min(titanic_df, age,n=2) %>% relocate(Port_of_Embarkation) # Otra forma
#¿De qué clase eran las dos personas más longevas?
arrange(titanic_df, desc(age),class) # First y Third

#11. Seleccionen a los pasajeros mujeres y sobre esto encuentren la edad máxima. 
female_pass <- filter(titanic_df, sex== "female")
slice_max(female_pass, age)
  # Las pasajeras mujeres mas longevas tenian 63
#¿Es el mismo resultado que calcular la edad máxima sobre todos los pasajeros?
#No, solo busca la edad maxima solamente en las entradas con sex = female

#12. Verificar: ¿qué edad máxima tienen los pasajeros considerados niños? ¿Es éste valor
#menor a la mínima edad de aquellos pasajeros adultos? ¡Ver cuál es la edad máxima
#de los niños!
child_pass <- filter(titanic_df, who == "child")
slice_max(child_pass, age)
#la edad maxima de los pass child es de 15

adult_pass <- filter(titanic_df, who != "child")
slice_min(adult_pass,age)
# La edad minima de plos pass adult es 16

#13. Elaborar gráficos de barras que permitan visualizar la cantidad de pasajeros con, según
# a. la cantidad de hermanos en el barco (variable siblings_of_the_passenger)
ggplot(data = titanic_df) + geom_bar(aes(x=siblings_of_the_passenger))
# b. si son hombres / mujeres / niñx (variable who)
ggplot(data = titanic_df) + geom_bar(aes(x=who))


#14. Realizar dos gráficos de barras que indiquen la cantidad de pasajeros hombres, mujeres
#y niñxs (de forma desagregada) que viajaban en cada clase. Es decir, hagan un gráfico
#en función de las clases desagregando por hombre/mujer/niñx y otro gráfico
#en función de hombre/mujer/niñx, desagregando por clase.
ggplot(data = titanic_df) + geom_bar(aes(x=who, fill = class))
ggplot(data = titanic_df) + geom_bar(aes(x=class, fill = who))


#15. Mostrar en dos gráficos de barras la cantidad de personas que viajaban solas y las
#que viajaban acompañadas, desagregando por las variables “who” y “class”
travel_alone <- filter(titanic_df, alone == TRUE)
ggplot(data = travel_alone) + geom_bar(aes(x=class, fill = who))
travel_not_alone <- filter(titanic_df, alone == FALSE)
ggplot(data = travel_not_alone) + geom_bar(aes(x=who, fill = class))
#¿podemos decir a simple vista si entre los viajeros acompañados había más personas pertenecientes a
#segunda clase que entre los viajeros solitarios?
  # No es posibe saberlo a simple vista para eso vamos a usar lo siguiente
ggplot(data = titanic_df) +
  geom_bar(mapping = aes(x=alone, fill = class), position = "dodge")



# ¿De las personas que viajan solas qué proporción son mujeres?
# Acá podemos ver la cantidad pero no el porcentaje
ggplot(data=titanic_df) + geom_bar(aes(x=alone, fill=who))
# Para ver el porcentaje de mujeres que viajan solas, agregamos position = fill
ggplot(data = titanic_df) +geom_bar(mapping = aes(x=alone, fill = who), position = 'fill')
# Para que el grafico sea mas claro usamos xlab() e ylab() para poner los labels (etiquetas)
ggplot(data = titanic_df) +
  geom_bar(mapping = aes(x=alone, fill = class), position = "fill")+
  xlab("Viaja Solo") +
  ylab("Proporción")

# Agrupaciones

titanic_groupwho <- group_by(titanic_df, who)



