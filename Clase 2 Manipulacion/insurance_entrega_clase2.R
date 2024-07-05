library(tidyverse)

insurance_df <- read_csv('C:\\Users\\Tomas\\Desktop\\Lic Ciencia de Datos\\Introduccion a Ciencia de Datos\\Datasets\\Clase 2\\insurance.csv')

spec(insurance_df)
glimpse(insurance_df)

ggplot(data = insurance_df) +
  geom_point(aes(x = age, y = charges , color = smoker))

# Persona que menos seguro medico paga
min(insurance_df$charges)
# Persona que mas seguro medico paga
max(insurance_df$charges)

# Cantidad de fumadores por region

ggplot(data = insurance_df) +
  geom_bar(aes(x = region, fill = smoker))

# De esta manera es mas facil ver los porcentajes de fumadores y no fumadores por region
ggplot(data = insurance_df) + geom_bar(aes(x=smoker, fill=region), position='fill')
