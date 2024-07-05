library(tidyverse)
library(datos)

datos::millas
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista))
?millas

#Autopista vs Cilindros

ggplot(data = millas) + 
  geom_point(mapping = aes(x = autopista, y = cilindros))

#Clase vs Traccion
#Porque no seria util este grafico?
    #Porque por ejemplo no es lo mismo lo que gasta un auto mediano con traccion en las 4 ruedas que una camioneta,
    #Depende del motor.

ggplot(data = millas) + 
  geom_point(mapping = aes(x = clase, y = traccion))

#Cilindrada vs Autopista
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase))


#Qué variables en millas son categóricas? ¿Qué variables son continuas?
    # Las variables son categoricas todas menos ciudad que es continua.


#¿Cómo puedes ver esta información cuando ejecutas millas?
    # Utilizando la funcion aes() de geom_point y coloreando segun las variables categoricas
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = clase))


#Asigna una variable continua a color, size, y shape. 
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = ciudad, size = autopista, shape = traccion))

#¿Cómo se comportan estas estéticas de manera diferente para variables categóricas y variables continuas?
  
#¿Qué ocurre si asignas o mapeas la misma variable a múltiples estéticas?
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = ciudad, size = ciudad, shape = ciudad))
    # Nos da un error en el geom point, una variable continua no puede ser mapeada a asthetic.

#¿Qué hace la estética stroke? ¿Con qué formas trabaja?
?geom_point 
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, stroke = cilindrada ))
    # Cambia el tamanio de los puntos en el grafico.

#¿Qué ocurre si se asigna o mapea una estética a algo diferente del nombre de una variable, como aes(color = cilindrada < 5)?
ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista, color = cilindrada > 5 ))
    # Nos da por resultado puntos con valores booleanos en el grafico y distinguidos por 2 colores
    # Los que son mayores que 5 en este caso de un color y menores que 5 con otro color.
#------------------------------------------------------------------------------------------

                                          #Facet Wrap

ggplot(data = millas) + 
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~ clase, nrow = 2)

                                          #Facet Grid

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ cilindros)
# Como se relaciona el facet grid con el siguiente grafico?
ggplot(data = millas) +
  geom_point(mapping = aes(x = traccion, y = cilindros))
    # Los espacios en blanco me estan indicando la cantidad de cilindros usados en base a la traccion?

#Que hace el siguiente codigo?
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ .)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(. ~ cilindros)
    # En el primer ejemplo comparamos la variable cilindrada vs autopista, y hacemos un grid segun la traccion
    # En el segundo ejemplo hacemos lo mismo pero hacemos el grid segun los cilindros
    # Por eso deciamos que los cilindros y la traccion estan relacionados en los espacios en blanco

#-------------------------------------------

#Mira de nuevo el primer gráfico en facetas presentado en esta sección:
  
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~ clase, nrow = 2)

#¿Cuáles son las ventajas de separar en facetas en lugar de aplicar una estética de color?
    # Las ventajas son que podemos ver mejor cada una de las variables que queremos analizar en el grafico,
    # Si usamos color podemos ver clara la diferencia, pero con tantos puntos en el grafico seria mucho mas complejo analizarlo.

#¿Cuáles son las desventajas? 
    #

#¿Cómo cambiaría este balance si tuvieras un conjunto de datos más grande?
    #

#Lee ?facet_wrap. ¿Qué hace nrow? ¿Qué hace ncol? 
    #

#¿Qué otras opciones controlan el diseño de los paneles individuales?
    #

#¿Por qué facet_grid() no tiene argumentos nrow y ncol?
    #

#Cuando usas facet_grid(), generalmente deberías poner la variable con un mayor número de niveles únicos en las columnas. 
#¿Por qué?
    #
