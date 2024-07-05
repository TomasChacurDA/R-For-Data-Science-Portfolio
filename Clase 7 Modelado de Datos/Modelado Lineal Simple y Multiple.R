
library(tidyverse)

df <- filter(iris, Species=='versicolor')

# L --> A <-- Otros

ggplot(data=df) +
  geom_point(aes(y=Petal.Width, x=Petal.Length))

# A = f(L, O)
# Â = f(L) --> predicción del modelo

# Elijo un modelo lineal simple
# Â = w0 + w1 * L; w0 y w1 son números (parámetros)
# O ~ Normal(0, sigma)

# Es equivalente a esto:
# formula = Petal.Width ~ Petal.Length

mod <- lm(data=df, formula=Petal.Width ~ Petal.Length)

summary(mod)

# Calculo las predicciones del modelo, Â
w0 <- -0.08429
w1 <- 0.33105
df <- df %>% 
  mutate(predic = w0 + w1 * Petal.Length)

# Más general y práctico
df <- modelr::add_predictions(data=df, model=mod)

ggplot(data=df) +
  geom_point(aes(y=Petal.Width, x=Petal.Length)) +
  geom_line(aes(y=pred, x=Petal.Length), color='red')


### Residuos
df <- df %>% 
  mutate(residuos = Petal.Width - pred)

df <- modelr::add_residuals(data=df, model=mod)

# Calculo error estándar de los residuos
mse <- sum(df$residuos**2) / (nrow(df) - 2)
rmse <- sqrt(mse)

# Saco las variables que metimos a mano
df <- select(df, -residuos2, -residuos, predic)


####
# Modelo lineal múltiple
####
mod2 <- lm(data=df, 
           formula=Petal.Width ~ Petal.Length + Sepal.Width)

# Elijo un modelo lineal simple
# Â = w0 + w1 * L + w2 * As; w0, w1 y w2 son números (parámetros)
# O ~ Normal(0, sigma)

summary(mod2)

# Más general y práctico
df <- modelr::add_predictions(data=df, model=mod2,
                              var='pred2')

# Data grid para modelo 1
grilla1 <- modelr::data_grid(data=df, Petal.Length) %>% 
  modelr::add_predictions(model=mod)

ggplot() +
  geom_point(data=df, 
             aes(y=Petal.Width, x=Petal.Length)) +
  geom_line(data=grilla1,
            aes(y=pred, x=Petal.Length), color='red')


# Data grid para modelo 2
grilla2 <- modelr::data_grid(data=df, 
                             Petal.Length,
                             Sepal.Width=modelr::seq_range(Sepal.Width, n=5)) %>% 
  modelr::add_predictions(mod2)


ggplot() +
  geom_point(data=df, 
             aes(y=Petal.Width, x=Petal.Length,
                 color=Sepal.Width)) +
  geom_line(data=grilla2,
            aes(y=pred, x=Petal.Length,
                color=Sepal.Width, group=Sepal.Width))



### Modelo 3
mod3 <- lm(data=df, 
           formula=Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length)

summary(mod3)

# Data grid para modelo 2
grilla3 <- modelr::data_grid(data=df, 
                             Petal.Length,
                             Sepal.Width=modelr::seq_range(Sepal.Width, n=5),
                             Sepal.Length=modelr::seq_range(Sepal.Length, n=5)) %>% 
  modelr::add_predictions(mod3)

ggplot() +
  geom_point(data=df, 
             aes(y=Petal.Width, x=Petal.Length,
                 color=Sepal.Width)) +
  geom_line(data=grilla3,
            aes(y=pred, x=Petal.Length,
                color=Sepal.Width, group=Sepal.Width))


### Modelo 4
ggplot(data=iris) +
  geom_point(aes(x=Petal.Length, 
                 y=Petal.Width, 
                 color=Species))

mod4 <- lm(data=iris,
           formula=Petal.Width ~ Petal.Length + Species)

# Â = w0_versicolor + w1 * L (si la flor es Versicolor)
# Â = w0_setosa + w1 * L (si la flor es Setosa)
# Â = w0_virginica + w1 * L (si la flor es Virginica)

summary(mod4)

grilla4 <- modelr::data_grid(data=iris, 
                             Petal.Length, Species) %>% 
  modelr::add_predictions(mod4)

ggplot() +
  geom_point(data=iris, 
             aes(y=Petal.Width, x=Petal.Length,
                 color=Species)) +
  geom_line(data=grilla4,
            aes(y=pred, x=Petal.Length,
                color=Species))



### Modelo 5
ggplot(data=iris) +
  geom_point(aes(x=Petal.Length, 
                 y=Petal.Width, 
                 color=Species))

mod5 <- lm(data=iris,
           formula=Petal.Width ~ Petal.Length + Sepal.Width + Species)

# Â = w0_versicolor + w1 * L + w2 * As(si la flor es Versicolor)
# Â = w0_setosa + w1 * L  + w2 * As(si la flor es Setosa)
# Â = w0_virginica + w1 * L  + w2 * As (si la flor es Virginica)

summary(mod5)
# Aparece el valor de w0_setosa como Intercept y las diferencias:
# w0_virginica - w0_setosa = SpeciesVirginica y w0_versicolor - w0_setosa = SpeciesVersicolor
#
# Esto nos permite evaluar fácilmente si hay diferencias significativas entre
# los valores de w0 de cada especie.

grilla5 <- modelr::data_grid(data=iris, 
                             Petal.Length, Species,
                             Sepal.Width=modelr::seq_range(Sepal.Width, n=5)) %>% 
  modelr::add_predictions(mod5)

ggplot() +
  geom_point(data=iris, 
             aes(y=Petal.Width, x=Petal.Length,
                 color=Sepal.Width)) +
  geom_line(data=grilla5,
            aes(y=pred, x=Petal.Length,
                color=Sepal.Width, group=Sepal.Width)) +
  facet_grid(~Species)


iris %>% modelr::add_predictions(model=mod5) %>% 
  modelr::add_residuals(model=mod5) %>% 
  ggplot() +
  geom_point(aes(x=pred, y=resid, color=Species)) +
  geom_hline(aes(yintercept=0))


# Si querés ver los valores de cada intercept, en lugar de las diferencias
mod6 <- lm(data=iris,
           formula=Petal.Width ~ Petal.Length + Sepal.Width + Species - 1)