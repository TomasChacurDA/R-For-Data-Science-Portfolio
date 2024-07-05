# Datos Relacionales
library(tidyverse)
library(dplyr)

# Importo el Dataset 'Players' con el encoding correcto
players <- read_csv('players.csv', locale = readr::locale(encoding = "latin1"))

# Importo el Dataset 'Games'         
games <- read_csv('games.csv')

# Importo el Dataset 'Shots'
shots <- read_csv('shots.csv')

# Importo el Dataset 'Leagues'
leagues <- read_csv('leagues.csv')

# Importo el Dataset 'Teams'
teams <- read_csv('teams.csv')

# Importo el Dataset 'Team Stats'
teamstats <- read_csv('teamstats.csv')

# Importo el Dataset 'Appearances'
appearances <- read_csv('appearances.csv')

# Claves primarias de los datasets

# Games DF, su Primary Key es gameID
count(games,gameID)

# Leagues DF, su Primary Key es leagueID
count(leagues,leagueID)

# Players DF, su Primary Key es playerID
count(players, playerID)

# Shots DF, su Foreing Key es 
#     gameID,shooterID, assisterID, shotType

# Teams DF, su Primary Key es teamID
count(teams, teamID)

# TeamStats tiene foreing key en
  # teamID, gameID, XGoals, goals, shotsOnTarget...

# Appearances tiene foreing keys tambien

### Por lo tanto NO es posible encontrar una clave primaria para cada uno de los dataset, solo para algunos.


### Encontrar los 10 equipos que mas goles metieron en todas las Ligas Europeas.
#   Para esto voy a usar el dataset teamstats que contiene informacion suficiente para responder esto.

top10eu <- group_by(teamstats, teamID) %>%# Agrupo el dataset elegido por teamID
  summarise(Goals = sum(goals)) %>% # Junto al summarize queda 1 df nuevo con teamID y Goals por TeamID
  slice_max(Goals, n = 10) # Me quedo con los 10 que mas goles metieron
  

# Ahora agregamos el nombre de cada uno de los 10 equipos al dataframe

top10eu <- left_join(top10eu, teams, by = NULL)

# Creamos un Grafico de Barras que muestre los goles por equipo.
  # Voy a usar 'geom_col' para que el alto de las barras represente los valores de los datos.
ggplot(top10eu) +
  geom_col(mapping = aes(x = name, y = Goals, fill = name)) +
  labs('Top 10 Equipos Goleadores') +
  xlab('Equipo') +
  ylab('Goles')


### Ahora vamos a encontrar los 10 equipos que mas tiros al arco hicieron en la Liga Europea

top10shotsEu <- group_by(teamstats, teamID) %>%
    summarise(Shots = sum(shotsOnTarget)) %>%
    slice_max(Shots, n = 10)

# Uno el dataset que cree con el de teams agregando una columna con los nombres de los equipos.
top10shotsEu <- left_join(top10shotsEu, teams, by = NULL)

# Creamos un Grafico de barras que muestre los datos.
ggplot(top10shotsEu) +
  geom_col(mapping = aes(x = name, y = Shots, fill = name)) +
  labs(title = 'Los 10 equipos que mas tiran al arco') +
  xlab('Equipo') +
  ylab('Tiros al Arco')
  

### Encontrar la liga que tiene la mejor relación tiro/goles.
# El dataset 'games' es el mas indicado para trabajar esto.
shot_goal_relationship <- group_by(games, gameID) %>%
  summarise(leagueID)

shot_goal_relationship <- left_join(shot_goal_relationship, teamstats, by = NULL)

# Armo un dataset que indica cantidad de tiros(shots), Goles(goals) por liga.
shots_goals_league <- group_by(shot_goal_relationship, leagueID) %>%
  summarise(Shots = sum(shots), Goals = sum(goals)) %>%
  slice_max(Shots, n = 5)

