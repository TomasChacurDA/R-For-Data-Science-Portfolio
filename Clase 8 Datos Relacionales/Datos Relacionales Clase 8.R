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

# Los 10 equipos que mas goles hicieron en Todas las Ligas de Europa

# Buscar un DF que tenga los TeamsID
# Buscar un DF que tenga los Goles por Equipo
# Agrupamos por equipo
top10EU <- group_by(teamstats, teamID)
# Sumamos goles por equipo
top10EU <- summarise(top10EU, Goals = sum(goals))
# Dejamos los top 10 goleadores
goleadores <- slice_max(top10EU,order_by = Goals,n = 10)

# Agregamos el nombre de cada equipo
goleadores_nombre <- inner_join(goleadores, teams, by = 'teamID')

# Imprimimos un grafico de barras para ver cantidad de goles por equipo
ggplot(goleadores_nombre) +
  geom_bar(aes(x = name, y = Goals), stat = 'identity')

# Hacemos lo mismo pero analizamos los tiros al arco
top10EUShots <- group_by(teamstats, teamID)
top10EUShots <- summarise(top10EUShots, Shots = sum(shots))
tiros_arco <- slice_max(top10EUShots,order_by = Shots,n = 10)
tiros_arco_nombre <- inner_join(tiros_arco, teams, by = 'teamID')
# Creamos el grafico de barras cantidad de tiros al arco por equipo
ggplot(tiros_arco_nombre) +
  geom_bar(aes(x = name, y = Shots), stat = 'identity')

######## Punto 10

mejor_relacion <- left_join(teamstats, games, by = 'gameID')

mrxl <- group_by(mejor_relacion,leagueID)
  
  
  
  
