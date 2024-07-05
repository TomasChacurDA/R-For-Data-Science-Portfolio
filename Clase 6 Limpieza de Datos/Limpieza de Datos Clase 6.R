# Clase 6 Limpieza de Datos
#------------------------------------------------------------------------------------------
#                                     Ver problemas de importacion
#------------------------------------------------------------------------------------------

# Cargo la libreria tidyverse
library(tidyverse)

# Importamos el dataframe de arbolado de la comuna 14 de CABA
df_arbol <- read_csv('C:/Users/Tomas/Desktop/Lic Ciencia de Datos/Introduccion a Ciencia de Datos/Clases/Clase 6 Limpieza de Datos/arbolado_comuna14.csv')


# Presten atención al mensaje de warning que aparece, ¿Pueden interpretar que está diciendo?
    
    # El mensaje Warning nos informa que hay uno o mas problemas de analisis de los datos.
    # Para ver donde se encuentra este error usamoe 'problems(nombre_dataframe).
    # Por lo visto hay 2 filas que tienen problema en la columna 6 del df.
    # La columna esperada deberia contener numeros decimales (a double), pero en su lugar contiene texto "45t" y "46ú".
    # Para solucionarlo edite el CSV directamente y lo volvi a importar a la variable df_arbol de vuelta.

# Ahora verifico los tipos de datos del Df

glimpse(df_arbol)

# Verifico en la consola que no hay errores en los tipos de datos al imprimirse el glimpse del df.

# Al ver el tipo de dato de la columna 'comuna' me doy cuenta que es un double(float) y no es necesario,
# ya que las comunas tienen un numero entero como identificador.

# Por lo tanto vamos a cambiarlo de double a entero

df_arbol <- df_arbol %>%
  mutate(comuna = as.integer(comuna))
# Hago un glimpse() en consola y veo que los datos cambiaron de double a integer


#------------------------------------------------------------------------------------------
#                                           Datos Repetidos
#------------------------------------------------------------------------------------------

# Para este ejemplo vamos a usar un data set de seguros medicos

df_seguros <- read_csv('C:/Users/Tomas/Desktop/Lic Ciencia de Datos/Introduccion a Ciencia de Datos/Clases/Clase 6 Limpieza de Datos/insurance.csv')

# Busco datos repetidos en el dataframe 
df_seguros %>%
  summarise(distintos = n_distinct(df_seguros), total = n())

# No se con certeza sin ver el dataset si es un dato repetido o son 2 personas con la misma caracteristica.
# Para estar seguro seria conveniente que el dataset tenga un identificador por persona como el DNI o ID.
# Siempre documentamos lo que hacemos con los datos para no generar confusiones o resultados equivocos.

# En este caso decido eliminar los datos duplicados y solo quedarme con los primeros en aparecer.
df_seguros <- unique(df_seguros)

# Unique me permite encontrar valores de atributos
unique(df_arbol$estado_plantera)

# Unique puede utilizarse tambien para encontrar valores mal ingresados en atributos que toman pocos valores

unique(df_arbol$nivel_plantera)
# Puedo ver ahora que hay valores mal ingresados por capitalizacion ('a nivel' / 'A Nivel', entre otros.)

# En casos como este la solucion puede ser simple, podriamos pasar todo a minusculas 
df_arbol <- df_arbol %>%
  mutate(nivel_plantera = str_to_lower(nivel_plantera))
  # Puedo hacer un unique de vuelta para chequear los cambios

# Otra forma de hacerlo mas elegante es la siguiente
df_arbol <- df_arbol %>%
  mutate(nivel_plantera = replace(nivel_plantera, nivel_plantera == 'A Nivel', 'A nivel'))
# Lo que pasa de esta forma es que deberia hacerlo para cada uno de los valores de los atributos que esten mal ingresados
# Igualmente es una buena practica, ninguna de las dos esta mal siempre depende de los datos que tengamos.

#------------------------------------------------------------------------------------------
#                                 Irregularidades en los Datos y Datos Faltantes
#------------------------------------------------------------------------------------------

# Podemos chequear cuantos valores faltantes hay en las observaciones de arboles en las columnas diametro y altura.

df_arbol %>% summarise(cant_arboles = n(),
                       na_altura = sum(is.na(altura_arbol)),
                       na_diametro = sum(is.na(diametro_altura_pecho))
                       )
# Veo que evidentemente si hay valores faltantes, por lo tanto puedo decidir
  # Si ignoro los datos faltantes, R no los tendra en cuenta a la hora de hacer cuentas o estadisticas.
  # Puedo reemplazar esos datos faltantes si nuestro dataset no es muy grande para no perder informacion.

# Decido reemplazarlos
# Entonces reemplazo los valores del diámetro por la MEDIA de todos los diámetros del data set. 
# Creo otro data frame para no pisar el original

df_arbol_2 <- df_arbol %>%
  mutate(diametro_altura_pecho 
         = replace(diametro_altura_pecho,
           is.na(diametro_altura_pecho),
           median(diametro_altura_pecho, na.rm = TRUE))
         )
# Esto parece tener sentido, pero porque no ver los valores que toma el diametro y altura en el df original?
# Que pasa si hago un histograma del dataframe original?  

ggplot(df_arbol) +
  geom_histogram(mapping = aes(x = diametro_altura_pecho))

### El diámetro está en centímetros y la altura en metros, o sea que no tiene sentido que
# haya árboles de diámetro cero, son valores erróneos. Cuando se calculó la media
# sobre todos los árboles para reemplazar los valores NA se usaron todos estos ceros para
# la cuenta. Por lo que la media usada no representa la realidad de los árboles. Primero
# deberíamos haber analizado las distribuciones de estos valores y DESPUÉS
# ver qué hacemos con los NA.

### Entonces busco primero los valores que tengan 0 y los reemplazo por NA,
# asi cuando calculemos la media ignora estos valores y es una media un poco mas real.
# Para esto uso el metodo 'replace'

df_arbol <- df_arbol %>%
  mutate(diametro_altura_pecho                 # Hago un mutate de una columna
         = replace(diametro_altura_pecho,      # Reemplaza esa columna
                   diametro_altura_pecho == 0, # Donde la misma columna tiene valores igual a 0
                   NA))                        # Cambia esos valores por NA 
                                               # Para que no se tengan en cuenta a la hr de trabajar y no afecten los calculos/estadisticas

# Verifico si los valores son NA y efectivamente ahora podemos seguir con el analisis

# Ahora si puedo reemplazar los valores medios

df_arbol_2 <- df_arbol %>%
  mutate(diametro_altura_pecho                 
         = replace(diametro_altura_pecho, 
                   is.na(diametro_altura_pecho), 
                   median(diametro_altura_pecho, na.rm = TRUE)) 
                   )
### Utilizando nuestro conocimiento del dominio del problema, podemos suponer que
# no todas las especies tienen la misma media de diámetro, si hacemos esto estamos
# asignando la media de TODOS los árboles de la comuna 14. Una mejor opción podría
# ser reemplazar los valores faltantes por la media de cada especie. Incluso podría hacerse
# por calle y/o por Comuna. Elijan alguna de las opciones y háganlo.

###9. Cuando se sientan satisfechos con todos los cambios y piensen que tienen el dataset
#listo para trabajar, hagan una gráfica de latitud vs. longitud para los árboles. Piensen
#qué esperan obtener antes de hacerlo. Cuando vean el resultado, discutan si es lo que
#esperaban y si necesitarían hacer alguna limpieza más con el data set antes de arrancar.
#De ser así, háganlo.
#TIP profesional: Pueden usar el código en mapa_caba.R para darle una base al
#gráfico y, si quieren, repetir el procedimiento con el data set de arbolado público lineal
#completo, eligiendo y trabajando con otra comuna.
#Parte 4. Documentación de cambios y decisiones
#Durante el proceso de limpieza y adecuación de los datos es necesario informar las decisiones
#tomadas a la hora de trabajar con estos datos. No se espera algo muy formal, pero es muy
#importante que estén reportados los cambios que se hicieron ya que se vuelven hipótesis de
#nuestro análisis.





