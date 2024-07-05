library(tidyverse)
library(modelr)

# Cómo se filtró para obtener el dataset sin 
# los fumadores ni los datos extremos
df_ns <- read_csv('datasets/insurance.csv') %>% 
  filter(smoker=='no')

mod <- lm(data=df_ns, formula = charges ~age)

# Gráfico de base con los datos
p <- ggplot(df_ns) + 
  geom_point(aes(x=age, y=charges, color=smoker))

# Creo un grilla y agrego las predicciones
grid <- data_grid(df_ns, age) %>% add_predictions(mod)
p + geom_line(data=grid, aes(x=age, y=pred), size=1)

# Filtramos en base a los residuos
df_ns <- df_ns %>% 
  add_residuals(mod) %>% 
  filter(resid < 2500)

# Salvar archivo
write_csv(df_ns, 'datasets/insurance_nofumador_filtrado.csv')
###

# Leer datos
df_ns = read_csv('datasets/insurance_nofumador_filtrado.csv') 

# Ver los tipos (notar en particular children)
glimpse(df_ns)

# Ver lo que pasa cuando se usa una variable float para el color
ggplot(df_ns) + geom_point(aes(x=age, y=charges, color=children))

# Cambiamos children a categórica
df_ns <- df_ns %>% mutate(children = factor(children))
glimpse(df_ns)

# Comparar con el plot anterior.
ggplot(df_ns) + geom_point(aes(x=age, y=charges, color=children))

## Modelo lineal de base
mod0 <- lm(data=df_ns, formula = charges ~ age)
summary(mod0)
coef(mod0)

df_ns %>% add_predictions(model=mod0) %>% 
  ggplot() + 
  geom_point(aes(x=age, y=charges)) +
  geom_line(aes(x=age, y=pred), color='red')

  # Geom_abline solo sirve si el modelo es una recta!  
  # geom_abline(aes(slope=coef(mod0)[[2]], intercept=coef(mod0)[[1]] -100), color='blue')

# Gráfico de los residuos
# Atajo!
plot(mod0)

# A mano
df_ns %>% add_predictions(model=mod0) %>% 
  add_residuals(model=mod0) %>% 
  ggplot(aes(x=pred, y=resid)) + geom_point() + geom_smooth() +
  geom_hline(aes(yintercept=0), color='red')

### Modelo polinomial (usando age + age**2)
mod <- lm(data=df_ns, formula=charges ~ poly(age, 2))
# Una versión alternativa, ligeramente diferente
#mod <- lm(data=df_ns, formula=charges ~age + I(age**2))

# Ver la matriz de diseño para entender qué covariables se crean
modelr::model_matrix(df_ns, charges ~ poly(age, 2))

# Ver los valores de los coeficientes y demás
summary(mod)

# Modelo con las predicciones
df_ns %>% add_predictions(model=mod, var='pred_cuad') %>% 
  ggplot() + 
  geom_point(aes(x=age, y=charges)) +
  geom_line(aes(x=age, y=pred_cuad), color='red')

# Una versión más eficiente, usando una grilla de valores
grid <- data_grid(df_ns, age) %>% add_predictions(mod, var='pred_cuad')

ggplot() + 
  geom_point(data=df_ns, aes(x=age, y=charges)) +
  geom_line(data=grid, aes(x=age, y=pred_cuad), color='red')

# Residuos
plot_res <- df_ns %>% 
  add_predictions(model=mod, var='pred_cuad') %>% 
  add_residuals(model=mod, var='resid_cuad') %>% 
  ggplot(aes(x=pred_cuad, y=resid_cuad)) + 
  geom_point() + 
  geom_smooth() +
  geom_hline(aes(yintercept=0), color='red')
plot_res

# Agregamos información de color a los residuos 
# Opción 1: repetir plot entero
df_ns %>% 
  add_predictions(model=mod, var='pred_cuad') %>% 
  add_residuals(model=mod, var='resid_cuad') %>% 
  ggplot(aes(x=pred_cuad, y=resid_cuad)) + 
  geom_point(aes(color=children)) + 
  geom_smooth() +
  geom_hline(aes(yintercept=0), color='red')

# Opción 2: modificar plot anterior (avanzado!)
plot_res$layers[[1]]$mapping <- aes(color=children)
plot_res

# Vemos un patrón con la cantidad de hijos.
# Confirmamos con un boxplot
df_ns %>% 
  add_residuals(model=mod, var='resid_cuad') %>% 
  ggplot(aes(x=children, y=resid_cuad)) + 
  geom_boxplot(aes(color=children))

# También se puede ver separando el gráfico de predicciones en facetas
ggplot(df_ns, aes(x=age, color=children)) + 
  geom_point(aes(y=charges)) +
  geom_line(data=add_predictions(grid, mod, var='pred_cuad'), 
            aes(y=pred_cuad), 
            color='black')+ 
  facet_wrap(~children)

# Demostración fuera de contexto de `facet_grid`
df_ns %>% add_predictions(mod) %>% add_residuals(mod) %>% 
  ggplot() + geom_point(aes(x=age, y=resid, color=region, shape=sex)) + 
  geom_hline(aes(yintercept=0)) + facet_grid(region ~ sex)

## MODELO 2

# Incluyo número de hijos (dos opciones)
# Como doble (es decir, dos variables continuas)
df_ns <- mutate(df_ns, children_=as.double(children))

mod2 <- lm(data=df_ns, formula=charges ~poly(age, 2) + children_)

# Vemos que en este caso hay un solo coeficiente extra.
# Se entiende que el costo extra de tener hijos es lineal
# (es lo mismo la diferencia entre 0 y 1 hijo, entre 1 y 2 hijos, 
# entre 2 y 3, etc.)
summary(mod2)

grid <- data_grid(df_ns, age, children_) %>% 
  add_predictions(mod2)

p <- ggplot(mapping=aes(x=age, y=charges, color=children_)) +
  geom_point(data=df_ns) +
  geom_line(data=grid, aes(y=pred, group=children_))
p


# Incluyo número de hijos (dos opciones)
# Como categórica (es decir, dos variables continuas)
mod2 <- lm(data=df_ns, formula=charges ~poly(age, 2) + children)

# Acá, tenemos más parámetros, hay un costo diferente para cada 
# cantidad de hijos
summary(mod2)

grid <- data_grid(df_ns, age, children) %>% add_predictions(mod2)

ggplot(mapping=aes(x=age, y=charges)) +
  geom_point(data=df_ns, mapping=aes(color=children)) +
  geom_line(data=grid, aes(y=pred, group=children))

ggplot(df_ns, aes(x=age)) + 
  geom_point(aes(y=charges, color=children)) +
  geom_line(data=grid, aes(y=pred)) + 
  facet_wrap(~children)

# Residuos
df_ns %>% 
  add_predictions(mod2) %>% 
  add_residuals(mod2) %>% 
  ggplot() + 
  geom_point(aes(x=age, y=resid, color=region)) + 
  geom_hline(aes(yintercept=0)) +
  facet_wrap(~sex)

# La cosa mejoró; los hijos se mezclaron, pero seguimos viendo 
# patrones!

box_res <- df_ns %>% 
  add_predictions(mod2) %>% 
  add_residuals(mod2) %>% 
  ggplot() + 
  geom_boxplot(aes(x=children, y=resid, color=children))
box_res


## MODELO 3

# Incluyo entonces sexo y región
mod3 <- lm(data=df_ns, 
           formula=charges ~poly(age, 2) + children + region + sex)
summary(mod3)

# Plot de las predicciones
df_ns %>% add_predictions(mod3) %>% add_residuals(mod3) %>% 
  ggplot() + geom_point(aes(x=age, y=charges, color=sex)) + 
  geom_line(aes(x=age, y=pred, color=sex)) +
  facet_grid(children ~ region)

# Plot de los residuos
df_ns %>% add_predictions(mod3) %>% add_residuals(mod3) %>% 
  ggplot() + geom_point(aes(x=age, y=resid, color=region, shape=sex)) + 
  geom_hline(aes(yintercept=0))

# Esos mismos Residuos vs. BMI
df_ns %>% add_predictions(mod3) %>% add_residuals(mod3) %>% 
  ggplot() + geom_point(aes(x=bmi, y=resid, color=region, shape=sex))

# Aparecen más patrones. Seguimos pelando la cebolla.

## MODELO 4

## Incluyo bmi!
mod4 <- lm(data=df_ns, formula=charges ~poly(age, 2) + children +
             region + sex + bmi)
summary(mod4)

# Residuos
df_ns %>% add_predictions(mod4) %>% add_residuals(mod4) %>% 
  ggplot() + geom_point(aes(x=age, y=resid, color=region, shape=sex)) + 
  geom_hline(aes(yintercept=0))


## Extra
### Predicción sobre datos no vistos
df_sincharges <- df_ns %>% select(-charges, -pred, -resid) %>% 
  add_predictions(mod4)




