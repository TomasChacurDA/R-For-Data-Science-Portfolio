library(tidyverse)
library(modelr)

# Gráfico inicial
p <- ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy))

# Modelo lineal simple
mod <- lm(hwy ~ displ, data=mpg)
summary(mod)

# Grilla de datos
grilla <- data_grid(mpg, displ) %>% 
  add_predictions(model=mod)

# Gráfico con modelo
p + geom_line(data=grilla, aes(x=displ, y=pred), color='red')
p

# Análisis de residuos
plot(mod)


mpg1 <- mpg %>% add_predictions(model=mod) %>% 
  add_residuals(model=mod) 

# Residuos en función de la predicción
ggplot(data=mpg1, aes(x=pred, y=resid)) +
  geom_point() + geom_hline(aes(yintercept=0)) +
  geom_smooth()

# Residuos en función de la variable predictora
ggplot(data=mpg1, aes(x=displ, y=resid)) +
  geom_point() + geom_hline(aes(yintercept=0)) +
  geom_smooth(color='red')

### Modelo lineal múltiple
# hwy = a0 + a1 * displ + a2 * displ**2
mod2 <-  lm(hwy ~ displ + I(displ**2), data=mpg)

# Equivalente
mod2 <-  lm(hwy ~ poly(displ, 2), data=mpg)
summary(mod2)  

grilla <- data_grid(mpg, displ) %>% 
  add_predictions(model=mod2)

p + geom_line(data=grilla, aes(x=displ, y=pred), color='red')

plot(mod2)

# Modelo con dos variables (continuas)
mod3 <-  lm(hwy ~ displ + year, data=mpg)
summary(mod3)

grilla <- data_grid(mpg, displ, year) %>% 
  add_predictions(model=mod3)

p + geom_line(data=grilla, 
              aes(x=displ, y=pred, group=year, color=year))

ggplot() + geom_point(data=mpg, 
                      aes(x=displ, y=hwy, color=year)) +
  geom_line(data=grilla, 
            aes(x=displ, y=pred, group=year, color=year))


# Modelo con dos variables (una continua y una discreta)
mod4 <-  lm(hwy ~ displ + class, data=filter(mpg, class!='2seater'))

grilla <- data_grid(mpg, displ, class) %>% 
  add_predictions(model=mod4)

ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy, color=class)) + 
  geom_line(data=grilla, aes(x=displ, y=pred, color=class))

### 

# hwy = a0 + a1 * displ + a2 * displ**2 + a3*clase1 + a4*clase2 + ...
mod5 <-  lm(hwy ~ poly(displ, 2, raw=TRUE) + class, data=mpg)

grilla <- data_grid(mpg, displ, class) %>% 
  add_predictions(model=mod5)

ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy, color=class)) + 
  geom_line(data=grilla, aes(x=displ, y=pred, color=class)) 


mod6 <-  lm(hwy ~ poly(displ, 2, raw=TRUE) + year + class, data=mpg)

grilla <- data_grid(mpg, displ, class, year) %>% 
  add_predictions(model=mod6)


ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy, color=class)) + 
  geom_line(data=grilla, aes(x=displ, y=pred, color=class)) +
  facet_wrap(~year)

