library(tidyverse)
library(modelr)

summary(iris)


df <- iris

df_versicolor <- df %>%
  filter(Species == 'versicolor')

ggplot(df_versicolor) +
  geom_point(mapping = aes(x = Petal.Length, y = Petal.Width))


# Modelo : Como se comporta la variable Target(Eje Y), en funcion de la variable Explicativa (Eje X)
# Este modelo tiene la forma de una funcion lineal en este caso Y = a + pendiente.X

mod <- lm(data = df_versicolor, Petal.Width ~ Petal.Length)

summary(mod)
# Intercept (Intercepto): -0.08429
  # Este es el valor de Petal.Width cuando Petal.Length es 0.
df <- df %>% add_predictions(model=mod)




# Representamos la linea del modelo
