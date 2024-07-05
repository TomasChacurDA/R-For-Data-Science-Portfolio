library(tidyverse)

titanic_df <- read_csv('C:/Users/Tomas/Desktop/Lic Ciencia de Datos/Introduccion a Ciencia de Datos/Clases/Clase 2 Manipulacion/CSVs/titanic.csv',locale = locale(decimal_mark = ","))

view(titanic_df)

spec(titanic_df)
glimpse(titanic_df)

# ¿Qué diferencias hay entre los outputs del comando spec(...) y glimpse(...)?
    # Spec da informacion sobre las columnas(variables) y sus tipos de dato.
    # Glimpse da informacion de las columnas, tipos de dato, cant de observaciones y valores de variables
# ¿Qué información comparten?
    # Tipos de dato de cada Variable
# ¿Cuántas observaciones hay?
    # 889
# Utilizando la información que se obtiene aplicando los comandos anteriores, respondan:
  # ¿Cuáles son las variables del dataset? ¿De qué tipos son?
    unique(titanic_df)
    # Double, Character, Logical.
  # Muchas veces los tipos de las variables se cargan con valores por defecto como ‘chr’ (char). 
  # Para las variables anteriores determinar si el tipo cargado es correcto o si consideran que debería ser otro.
    # Las variables siblings_of_the_passenger y parents_or_children las cambiaria por enteros ya que no se puede tener 1.5 familiares por ejemplo.
    # Se podria usar como Logical la variable survived.
  # ¿Qué variables consideran que podrían utilizarse como categóricas?
    # Class, Who, Sex, Port of Embarkation, Alone, Survived.
    
# Seleccione todos los valores de la columna age (edad) ejecutando el comando titanic_df$age, van a ver que se los muestra todos en la consola. 
# Si hacen min(titanic_df$age) están ejecutando la función mínimo sobre todos esos números.
# Háganlo y traten de explicar el resultado.
    
titanic_df$age

min(titanic_df$age, na.rm = T)

# El valor minimo es un valor NA, removemos los NA de la columna y vemos un valor un poco dudoso pero podemos decir que es un bebe de 4 meses de edad.

# Ahora saquemos la observacion completa con los valores minimos 
slice_min(titanic_df, order_by=age, n = 5) %>%
  relocate(survived)

slice_max(titanic_df, order_by = age, n= 5) %>%
  relocate(survived)


#a. ¿Cuál fue el boleto más barato (con fare menor)? ¿Y el más caro?.
   slice_max(titanic_df, order_by = fare)
   # 512 dolares/euros es el mas Caro
   slice_min(titanic_df, order_by = fare)
   # 4.01 dolares/euros es el mas Barati
#b. ¿Cuántas categorías de clase (class) había?.
    unique(titanic_df$class)
    # Habia 3 clases. First, Second y Third.
#c. ¿Cuáles eran los puertos desde donde embarcaron (Port_of_Embarkation)? 
    unique(titanic_df$Port_of_Embarkation)
    # S C Q
    
# 9. ¿Desde qué puerto embarcaron las dos personas más jóvenes que estaban en el barco?.
    arrange(titanic_df, Port_of_Embarkation, age)
    # Las personas mas jovenes embarcaron desde el puerto C
# 10. ¿De qué clase eran las dos personas más longevas?.
    arrange(titanic_df,desc(age),class)
    # Las dos personas mas jovenes son de 80 y 74 anios y son de primera y tercera clase respectivamente.

    