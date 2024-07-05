# TP GRUPO 16
#version 29/05 15:20hs

#########################################
### ------- Librerias ----------------####
#########################################
library(ggmap)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(treemap)
library(arules)
library(leaflet)
library(arulesViz)

#########################################
### ------- Datasets ----------------####
#########################################

los_angeles_crimes <- read_csv('Crime_Data_from_2020_to_Present.csv')
edades_la <- read_csv("edades_sexo_los_angeles_2022.csv")
census <- read_csv("Census_Data_by_Neighborhood_Council.csv")

############################################################
### ------- Fin de la Carga de Datasets ----------------####
############################################################

################################################################################
### ------- Limpieza de Datos : Dataset Crimenes ----------------####
################################################################################

### Cambiamos los nombres de Variables para facilitar la manipulación de datos. 

los_angeles_crimes_fixed <- los_angeles_crimes %>%
  rename(DR_NO, crime_id = DR_NO, `Date Rptd`, date_reported = `Date Rptd`, 
         `DATE OCC`, date_occurred = `DATE OCC`, `TIME OCC`, time_occurred = `TIME OCC`, 
         AREA, area = AREA, `AREA NAME`, area_name = `AREA NAME`, `Rpt Dist No`, 
         report_district_number = `Rpt Dist No`, `Part 1-2`, crime_level = `Part 1-2`, `Crm Cd`, 
         crime_code = `Crm Cd`, `Crm Cd Desc`, crime_description = `Crm Cd Desc`, 
         Mocodes, modus_operandi_code = Mocodes, `Vict Age`, victim_age = `Vict Age`, 
         `Vict Sex`, victim_gender = `Vict Sex`, `Vict Descent`, victim_ethnic = `Vict Descent`, 
         `Premis Cd`, premises_code = `Premis Cd`, `Premis Desc`, premises_description = `Premis Desc`, 
         `Weapon Used Cd`, weapon_used_code = `Weapon Used Cd`, `Weapon Desc`, 
         weapon_description = `Weapon Desc`, Status, crime_status = Status, `Status Desc`, 
         crime_status_desc = `Status Desc`, `Crm Cd 1`, additional_crime_code1 = `Crm Cd 1`,`Crm Cd 2`, 
         additional_crime_code2 = `Crm Cd 2`,`Crm Cd 3`, 
         additional_crime_code3 = `Crm Cd 3`,`Crm Cd 4`, 
         additional_crime_code4 = `Crm Cd 4`, LOCATION, crime_location = LOCATION, `Cross Street`, 
         cross_street = `Cross Street`, LAT, lat = LAT, LON, lon = LON)

los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>% 
  mutate(crime_status_desc= replace(crime_status_desc, crime_status_desc == "UNK", "Citizen Complaint"),
         crime_status_desc= replace(crime_status_desc, crime_status_desc == "Invest Cont", "Investigation Continues"),
         crime_status_desc= replace(crime_status_desc, crime_status_desc == "Juv Arrest", "Juvenile Arrest"),
         crime_status_desc= replace(crime_status_desc, crime_status_desc == "Juv Other", "Juvenile Other"))


### Tenemos 2 columnas sin valores que no influyen en el analisis, las eliminamos del dataset.
los_angeles_crimes_fixed <- subset(los_angeles_crimes_fixed, select =
                                     -c(additional_crime_code3,additional_crime_code4))

# Crime ID a Integer
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(crime_id = as.integer(crime_id))

# Time Ocurred a numeric
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(time_occurred = as.numeric(time_occurred))

# Area a Integer
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(area = as.integer(area))

# Report district number a Integer
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(report_district_number = as.integer(report_district_number))

### Veamos si tenemos datos repetidos en el dataset.
los_angeles_crimes_fixed %>%
  summarise(distintos=n_distinct(los_angeles_crimes_fixed), total=n())
# Por lo visto no tenemos datos repetidos, el dataset esta bastante limpio.

### Las calles que cortan donde ocurrió el crimen tienen muchos valores NA, pero no es relevante.
los_angeles_crimes_fixed %>% summarise(cant_crimenes = n(),
                                       na_street = sum(is.na(cross_street)))

# Vemos que hay muchos valores de edades que tienen 0 como valor.
los_angeles_crimes_fixed %>% summarise(cant_crimenes = n(),
                                       zero_age = sum(victim_age == 0))

# Cambiamos las edades 0 <= a NA.
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(victim_age = replace(victim_age, victim_age <= 0, NA))

# Cambio de hora de los angeles crime fixed 

#Sacar hora de date_occurred y date_reported
los_angeles_crimes_fixed[c('date_occurred', 'houre_date_occurred')] <- str_split_fixed(los_angeles_crimes_fixed$date_occurred, ' ', 2)
los_angeles_crimes_fixed[c('date_reported', 'houre_date_reported')] <- str_split_fixed(los_angeles_crimes_fixed$date_reported, ' ', 2)
los_angeles_crimes_fixed <- subset(los_angeles_crimes_fixed, select = -c(houre_date_reported,houre_date_occurred))

#cambio de hora de los angeles crime fixed

# Añadir ceros a la izquierda para asegurar que todos tengan 4 dígitos
los_angeles_crimes_fixed$time_occurred <- sprintf("%04d", as.integer(los_angeles_crimes_fixed$time_occurred))

# Convertir la columna al formato hh:mm
los_angeles_crimes_fixed$time_occurred <- format(strptime(los_angeles_crimes_fixed$time_occurred, format = "%H%M"), format = "%H:%M")

# Extraer solo la hora en formato hh
los_angeles_crimes_fixed$time_occurred <- substr(los_angeles_crimes_fixed$time_occurred, 1, 2)


# Convertir los valores de victim_gender a otros mas legibles.
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(victim_gender = case_when(
    victim_gender == 'F' ~ 'Female',
    victim_gender == 'M' ~ 'Male',
    victim_gender == 'H' ~ 'Other',
    victim_gender == 'X' ~ 'Other',
    TRUE ~ victim_gender
  ))

# Crear Categorías para las armas utilizadas

weapon_category <- function(code) {
  if (is.na(code)) {
    return("No weapon")
  } else if (code >= 100 & code <= 125) {
    return("Firearm")
  } else if (code >= 200 & code <= 223) {
    return("Sharp Object")
  } else if (code >= 300 & code <= 312) {
    return("Blunt Object")
  } else if (code == 400) {
    return("Strong Arms")
  } else if (code >= 500 & code <= 516) {
    return("Other Weapon")
  } else {
    return("No weapon")
  }
}

los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  rowwise() %>%
  mutate(weapon_description = weapon_category(weapon_used_code))

#######################################

# Definir la lista de ubicaciones
ubicacion <- los_angeles_crimes_fixed$premises_description

# Función ampliada para asignar categorías
asignar_categoria <- function(ubicacion) {
  if (grepl('APARTMENT|DWELLING|HOUSE|CONDO|YARD|HOTEL|GROUP HOME|SRO|RESIDENCE|MOTEL|TRANSITIONAL HOUSING|HALFWAY HOUSE|FOSTER HOME|SHORT-TERM VACATION RENTAL|PORCH|BALCONY|GARAGE|CARPORT|DRIVEWAY|MOBILE HOME|TRAILERS|CONSTRUCTION TRAILERS|RV|MOTORHOME|ABANDONED BUILDING|ABANDONED HOUSE|PROJECT|TENEMENT|PUBLIC HOUSING|MISSIONS|SHELTERS|NURSING|CONVALESCENT|RETIREMENT HOME|HOSPICE', ubicacion, ignore.case = TRUE)) {
    return('Homes and Accommodations')
  } else if (grepl('RESTAURANT|MARKET|BAR|CAFE|FOOD|FAST FOOD|COFFEE SHOP|NIGHT CLUB|MINI-MART|GROCERY STORE|LIQUOR STORE|TOBACCO SHOP|DRUG STORE|PHARMACY|CLOTHING STORE|DEPARTMENT STORE|BEAUTY SUPPLY STORE|OTHER STORE|CELL PHONE STORE|JEWELRY STORE|TOBACCO SHOP|DISCOUNT STORE|AUTO DEALERSHIP|AUTO SUPPLY STORE|PET STORE|NURSERY|FLOWER SHOP|ELECTRONICS STORE|MEMBERSHIP STORE|DIY CENTER|FURNITURE STORE|BOOK STORE|GUN|SPORTING GOODS|CHECK CASHING|SURPLUS SURVIVAL STORE|RECORD-CD MUSIC|COMPUTER GAME STORE|VIDEO RENTAL STORE', ubicacion, ignore.case = TRUE)) {
    return('Retail and Food Services')
  } else if (grepl('SCHOOL|CHURCH|HOSPITAL|POLICE|FIRE|JAIL|LIBRARY|UNIVERSITY|COLLEGE|JUNIOR COLLEGE|ELEMENTARY SCHOOL|HIGH SCHOOL|JUNIOR HIGH SCHOOL|TRADE SCHOOL|SPECIALTY SCHOOL|OTHER PLACE OF WORSHIP|SYNAGOGUE|TEMPLE|DAY CARE|PRESCHOOL|MISSIONS|SHELTERS|MUSEUM|GOVERNMENT FACILITY|ABORTION CLINIC|ABORTION FACILITY|METHADONE CLINIC|MEDICAL MARIJUANA FACILITIES|POST OFFICE|DEPT OF DEFENSE FACILITY|CEMETARY|NURSERY|TRANSITIONAL HOUSING|HALFWAY HOUSE|FOSTER HOME', ubicacion, ignore.case = TRUE)) {
    return('Public and Government Services')
  } else if (grepl('BUS|TRAIN|AIRPORT|TRANSIT|STATION|MTA|LA UNION STATION|METROLINK TRAIN|MTA PROPERTY|BUS DEPOT|TAXI|AIRCRAFT|MTA BUS|SUBWAY|LIGHT RAIL|OTHER INTERSTATE|CHARTER BUS|GREYHOUND|BUS STOP|LAYOVER|TRAIN DEPOT|TERMINAL|TRAM|STREETCAR|VEHICLE|TRUCK|CAR|AUTO|DEALERSHIP|GAS STATION|SERVICE|GARAGE|PARKING|VEHICLE STORAGE LOT|TOW YARD|DELIVERY SERVICE', ubicacion, ignore.case = TRUE)) {
    return('Transportation Facilities')
  } else if (grepl('PARK|PLAYGROUND|BEACH|PLAZA|STREET|SIDEWALK|ALLEY|PUBLIC RESTROOM|UNDERPASS|BRIDGE|STAIRWELL|ESCALATOR|POOL|BASKETBALL COURTS|HAND BALL COURTS|SKATEBOARD FACILITY|SKATEBOARD PARK|TUNNEL|PEDESTRIAN OVERCROSSING|VACANT LOT|TRANSIENT ENCAMPMENT|TRASH CAN|TRASH DUMPSTER|MASS GATHERING LOCATION', ubicacion, ignore.case = TRUE)) {
    return('Public Spaces')
  } else if (grepl('FACTORY|WAREHOUSE|PLANT|INDUSTRIAL|MANUFACTURING COMPANY|GARMENT MANUFACTURER|CHEMICAL STORAGE|STORAGE SHED|TELECOMMUNICATION FACILITY|PIPE|SEWAGE FACILITY|ENERGY PLANT|WATER FACILITY', ubicacion, ignore.case = TRUE)) {
    return('Industrial Zones')
  } else if (grepl('THEATER|STADIUM|CLUB|CASINO|ENTERTAINMENT|BOWLING ALLEY|ARCADE|GAME ROOM|VIDEO GAMES|SPORTS VENUE|AMUSEMENT PARK|HORSE RACING|SANTA ANITA PARK|MUSEUM|CULTURAL SIGNIFICANCE|MONUMENT|STUDIO|FILM|PHOTOGRAPHIC|MUSIC|MUSCLE BEACH|SPORTS ARENA|COLISEUM', ubicacion, ignore.case = TRUE)) {
    return('Entertainment Venues')
  } else if (grepl('CLINIC|HOSPITAL|SPA|SALON|NURSING|MEDICAL|DENTAL|OFFICES|HEALTH SPA|GYM|VISION CARE FACILITY|MASSAGE PARLOR|MEDICAL MARIJUANA FACILITIES|FACILITIES|VETERINARIAN|ANIMAL HOSPITAL', ubicacion, ignore.case = TRUE)) {
    return('Health and Beauty Services')
  } else if (grepl('OFFICE|BUSINESS|COMPANY|CORPORATE|CENTER|OFFICE BUILDING|OPTICAL OFFICE|COMPUTER SERVICES|REPAIRS|SALES|CLEANER|LAUNDROMAT|LAUNDRY ROOM', ubicacion, ignore.case = TRUE)) {
    return('Offices and Businesses')
  } else if (grepl('BANK|CREDIT UNION|FINANCE|LOAN|ATM|AUTOMATED TELLER MACHINE|BANKING INSIDE MARKET|BANK DROP BOX|SAVINGS|LOAN|DRIVE THRU BANKING', ubicacion, ignore.case = TRUE)) {
    return('Banks')
  } else if (grepl('EMERGENCY|AMBULANCE|FIRE STATION|EMERGENCY SERVICES', ubicacion, ignore.case = TRUE)) {
    return('Emergency Services')
  } else if (grepl('POST OFFICE|MAIL BOX|DELIVERY SERVICE|COURIERS', ubicacion, ignore.case = TRUE)) {
    return('Postal and Courier Services')
  } else {
    return('Unidentified Location')
  }
}

# Aplicar la función a cada ubicación
los_angeles_crimes_fixed$premises_description <- sapply(ubicacion, asignar_categoria)

####################################################

# Definir la función group_crime
group_crime <- function(description) {
  crime_patterns <- list(
    "Robbery" = "ROBBERY|PURSE SNATCHING|ATTEMPTED ROBBERY",
    "Assault" = "ASSAULT|BATTERY",
    "Vandalism" = "VANDALISM",
    "Burglary" = "BURGLARY",
    "Theft" = "THEFT|SHOPLIFTING|EMBEZZLEMENT|BIKE - STOLEN|PETTY THEFT|PURSE SNATCHING",
    "Fraud" = "FRAUD|FORGERY|EMBEZZLEMENT|CREDIT CARDS|DOCUMENT WORTHLESS|INSURANCE FRAUD",
    "Drug Related" = "DRUG",
    "Weapon Related" = "WEAPON|DISCHARGE FIREARMS|FIREARMS EMERGENCY PROTECTIVE ORDER|BRANDISH WEAPON|SHOTS FIRED",
    "Homicide" = "HOMICIDE|MANSLAUGHTER",
    "Sex Offense" = "SEX|LEWD|RAPE|SODOMY|INCEST|BEASTIALITY|CHILD PORNOGRAPHY|PEEPING TOM|ORAL COPULATION|SEXUAL PENETRATION",
    "Kidnapping" = "KIDNAPPING|CHILD STEALING",
    "Arson" = "ARSON",
    "Embezzlement" = "EMBEZZLEMENT",
    "Disorderly Conduct" = "DISORDERLY CONDUCT|DISTURBING THE PEACE|DRUNK ROLL",
    "Vehicle Related" = "VEHICLE|DRIVING WITHOUT OWNER CONSENT|THEFT FROM MOTOR VEHICLE",
    "Forgery" = "FORGERY",
    "Extortion" = "EXTORTION",
    "Other" = "OTHER MISCELLANEOUS CRIME|OTHER ASSAULT|FAILURE TO YIELD|TRESPASSING|VIOLATION OF COURT ORDER|VIOLATION OF RESTRAINING ORDER|CRUELTY TO ANIMALS|ILLEGAL DUMPING|BRIBERY|PROWLER|BIGAMY|CONTEMPT OF COURT|CONSPIRACY|PANDERING|PIMPING|BOMB SCARE|THREATENING PHONE CALLS|FALSE POLICE REPORT|DEFRAUDING INNKEEPER|FALSE IMPRISONMENT|SHOTS FIRED AT MOVING VEHICLE|UNAUTHORIZED COMPUTER ACCESS"
  )
  
  for (category in names(crime_patterns)) {
    pattern <- crime_patterns[[category]]
    if (grepl(pattern, description, ignore.case = TRUE)) {
      return(category)
    }
  }
  return("Other")
}

# Aplicar la función al dataframe usando dplyr
los_angeles_crimes_fixed <- los_angeles_crimes_fixed %>%
  mutate(crime_category = sapply(crime_description, group_crime))

los_angeles_crimes_fixed %>%
  group_by(crime_category)%>%
  summarise(count = n())%>%
  ungroup()


#write.csv(los_angeles_crimes_fixed, file = "los_angeles_crimes_fixed_final.csv")

################################################################################
### ------- Fin de la Limpieza de Datos : Dataset Crimenes ----------------####
################################################################################

################################################################################
### ------------ Limpieza de Datos : Dataset Censo ----------------    ####
################################################################################

# Elimino la columna hispanic pop y total_pop
census_fixed <- census %>% 
  select(-Hispanic_pop)


# Empiezo a juntar las mismas zonas
census_fixed <- census_fixed %>%
  mutate(NC_Name = case_when(
    NC_Name %in% c('EMPOWERMENT CONGRESS CENTRAL AREA NDC', 'EMPOWERMENT CONGRESS SOUTHWEST AREA NDC', 'EMPOWERMENT CONGRESS WEST AREA NDC', 'MID CITY NC', 'PARK MESA HEIGHTS CC', 'UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS, WEST ADAMS, AND JEFFERSON PARK COMMUNITY', 'WEST ADAMS NC') ~ 'Southwest',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('DOWNTOWN LOS ANGELES', 'WESTLAKE NORTH NC', 'WESTLAKE SOUTH NC') ~ 'Central',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU)') ~ '77th Street',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('SHERMAN OAKS NC', 'VAN NUYS NC') ~ 'Van Nuys',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('BOYLE HEIGHTS NC', 'LA-32 NC') ~ 'Hollenbeck',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('BEL AIR-BEVERLY CREST NC', 'P.I.C.O. NC', 'WEST LOS ANGELES NC', 'WESTSIDE NC', 'WESTWOOD NC') ~ 'West LA',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('GREATER WILSHIRE NC', 'MID CITY WEST CC', 'WILSHIRE CENTER - KOREATOWN NC') ~ 'Wilshire',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('CENTRAL SAN PEDRO NC', 'COASTAL SAN PEDRO NC', 'HARBOR CITY NC', 'HARBOR GATEWAY NORTH NC', 'HARBOR GATEWAY SOUTH NC', 'NORTHWEST SAN PEDRO NC', 'WILMINGTON NC') ~ 'Harbor',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('CENTRAL HOLLYWOOD NC', 'EAST HOLLYWOOD NC', 'HISTORIC CULTURAL NC', 'HOLLYWOOD HILLS WEST NC', 'HOLLYWOOD STUDIO DISTRICT NC', 'HOLLYWOOD UNITED NC') ~ 'Hollywood',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('ARLETA NC', 'FOOTHILL TRAILS DISTRICT NC', 'PACOIMA NC', 'SUN VALLEY AREA NC', 'SUNLAND-TUJUNGA NC', 'SYLMAR NC') ~ 'Foothill',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('CANOGA PARK NC', 'ENCINO NC', 'LAKE BALBOA NC', 'RESEDA NC', 'TARZANA NC', 'WEST HILLS NC') ~ 'West Valley',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('EMPOWERMENT CONGRESS SOUTHEAST AREA NDC', 'WATTS NC') ~ 'Southeast',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('ARROYO SECO NC', 'ATWATER VILLAGE NC', 'EAGLE ROCK NC', 'ELYSIAN VALLEY RIVERSIDE NC', 'EMPOWERMENT CONGRESS NORTH AREA NDC', 'GLASSELL PARK NC', 'GREATER CYPRESS PARK NC', 'GREATER ECHO PARK ELYSIAN NC', 'HERMON NC', 'HISTORIC HIGHLAND PARK NC', 'LINCOLN HEIGHTS NC', 'LOS FELIZ NC', 'SILVER LAKE NC') ~ 'Northeast',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('MACARTHUR PARK NC', 'PICO UNION NC', 'RAMPART VILLAGE NC') ~ 'Rampart',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('CENTRAL ALAMEDA NC', 'SOUTH CENTRAL NC', 'VOICES OF 90037', 'ZAPATA KING NC') ~ 'Newton',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('GREATER TOLUCA LAKE NC', 'GREATER VALLEY GLEN COUNCIL', 'MID-TOWN NORTH HOLLYWOOD NC', 'NC VALLEY VILLAGE', 'NOHO WEST NC', 'NORTH HOLLYWOOD NORTHEAST NC', 'STUDIO CITY NC') ~ 'N Hollywood',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('WINNETKA NC', 'WOODLAND HILLS-WARNER CENTER NC') ~ 'Topanga',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('DEL REY NC', 'MAR VISTA CC', 'NC WESTCHESTER/PLAYA', 'PALMS NC', 'SOUTH ROBERTSON NC', 'VENICE NC') ~ 'Pacific',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('MISSION HILLS NC', 'PANORAMA CITY NC') ~ 'Mission',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('OLYMPIC PARK NC') ~ 'Olympic',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ), NC_Name = case_when(
    NC_Name %in% c('CHATSWORTH NC', 'GRANADA HILLS NORTH NC', 'GRANADA HILLS SOUTH NC', 'NORTH HILLS EAST', 'NORTH HILLS WEST NC', 'NORTHRIDGE EAST', 'NORTHRIDGE SOUTH NC', 'NORTHRIDGE WEST', 'PORTER RANCH NC') ~ 'Devonshire',
    TRUE ~ NC_Name  # Mantener cualquier otro valor de ubicación sin cambio
  ))

# Dejamos el total de personas para cada una de las zonas.
census_fixed <- census_fixed %>%
  group_by(NC_Name) %>%
  summarise_all(sum)

census_fixed <- census_fixed %>%
  mutate_at(c('Total Population', 'White_pop', 'Black_pop', 'Ameri_es_pop', 'Asian_pop', 'Hawn_pi_pop', 'Other_pop', 'Multi_pop', 'In_Poverty', 'Owner_occ', 'Renter_occ'), as.integer)

census_fixed <- census_fixed %>%
  rename(NC_Name, area_name = NC_Name, `Total Population`, Total_pop = `Total Population`)

################################################################################
### ------------ Fin limpieza de Datos : Dataset Censo ----------------    ####
################################################################################

################################################################################
### ------------ Limpieza de Datos : Dataset Edades ----------------    ####
################################################################################

# Cambio los nombres
edades_la_fixed <- edades_la %>% 
  rename("Label (Grouping)", groups = "Label (Grouping)", 
         'Los Angeles city, California!!Total!!Estimate', total = 'Los Angeles city, California!!Total!!Estimate',
         'Los Angeles city, California!!Male!!Estimate', male_count = 'Los Angeles city, California!!Male!!Estimate',
         'Los Angeles city, California!!Female!!Estimate', female_count = 'Los Angeles city, California!!Female!!Estimate')

# Elimino las columnas que no me sirven
edades_la_fixed <- subset(edades_la_fixed, select = -c(`Los Angeles city, California!!Total!!Margin of Error`,`Los Angeles city, California!!Percent!!Margin of Error`, 
                       `Los Angeles city, California!!Percent!!Estimate`))

# Elimino las filas que no me sirven
edades_la_fixed <- edades_la_fixed[-c(21:42), ]
edades_la_fixed <- edades_la_fixed[-c(1:2), ]


# Convertí los números a enteros y formato ingles
edades_la_fixed <- edades_la_fixed %>% 
  mutate(total = as.numeric(gsub(",", "", total)), 
         male_count = as.integer(gsub(",", "", male_count)), 
         female_count = as.integer(gsub(",", "", female_count)))


################################################################################
### ------------ Fin limpieza de Datos : Dataset Edades ----------------    ####
################################################################################

################################################################################
###        ------------ Filtrado de datos para analisis ----------------    ####
################################################################################

# Filtrado de crimenes, generando nuevo df con solo los arrestos

la_arrest <- los_angeles_crimes_fixed %>% 
  filter(crime_status != "IC", crime_status != "AO", crime_status != "JO", crime_status != "CC")

###############################

# Identificacion de los tipos de crimenes cuya cantidad de observaciones 
#este por arriba de la media
top_median_crimes <- la_arrest %>%
  count(crime_category) %>%
  arrange(desc(n))
#top_median_crimes <- filter(top_median_crimes, n > mean(top_median_crimes$n))

# Filter la_arrest for only the top 10 crimes
la_arrest_top <- la_arrest %>%
  filter(crime_category %in% top_median_crimes$crime_category)


##### Grafico 1 ######

ggplot(top_median_crimes, aes(x = reorder(crime_category, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Crime Description", y = "Count", title = "Top Arrest Crime Descriptions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold")) +
  coord_flip(ylim = c(0, 11000))

ggplot(top_median_crimes, aes(x = reorder(crime_category, n), y = n)) +
  geom_bar(stat = "identity") +
  scale_y_log10() + 
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  labs(x = "Crime Description", y = "Count (log scale)", title = "Top Arrest Crime Descriptions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"))+
  coord_flip(ylim = c(1, 35000))


##### Grafico 2 Normalizados ######

#Normalizado de crimenes cada 100 habitantes

normalize_crime_area <- la_arrest_top %>%
  group_by(area_name) %>%
  summarise(count = n()) %>%
  ungroup() 

normalize_crime_area <- left_join(normalize_crime_area, census_fixed, by = NULL)

normalize_crime_area <- normalize_crime_area[c(1,2,3)]
normalize_crime_area <- normalize_crime_area %>%
  mutate(crime_every_tenk = (count/Total_pop)*100)

ggplot(normalize_crime_area, aes(x = reorder(area_name, -crime_every_tenk), y = crime_every_tenk)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", crime_every_tenk)), vjust = -1 , size = 5) +
  labs(x = "Área", y = "Crimes evry 100 people", title = "Crimes every 100 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Grafico 3 Map tree  ######
# Definir la lista de ubicaciones

# Aplicar la función a cada ubicación
categorias <- sapply(ubicacion, asignar_categoria)

# Contar la frecuencia de cada categoría
frecuencia_categorias <- table(categorias)


### Treemap Establecimientos con mayor frecuencia de Robos que Terminaron en Arresto
df_treemap <- as.data.frame(frecuencia_categorias)
colnames(df_treemap) <- c("Categoría", "Frecuencia")
treemap(df_treemap, index = "Categoría", vSize = "Frecuencia", title = "En que sitios ocurren mas frecuentemente los delitos que terminan en arrestos?", 
        border.col=c("black","white"),             
        border.lwds=c(2,2))

##### Grafico 4 Mapa crimen por zona######

# Define a color palette
palette <- colorFactor("Set1", levels = unique(la_arrest$area_name))

# Read the dataset
# Assuming the dataset is in a CSV file
la_map <- leaflet(data = la_arrest) %>%
  addTiles() %>%
  setView(lng = -118.2437, lat = 34.0522, zoom = 10) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   popup = ~as.character(area_name), 
                   radius = 3,
                   stroke = FALSE, 
                   fillOpacity = 1,  # Adjust the opacity if needed 0.03
                   color = ~palette(area_name))

# Print the map object
print(la_map)

# Suponiendo que 'la_arrest' es tu dataframe
la_arrest_filtered <- la_arrest %>%
  filter(lat != 0 & lon != 0)

# Crear el gráfico con ggplot2
p <- ggplot(la_arrest_filtered, aes(x = lon, y = lat)) +
  geom_point(aes(color = area_name, text = area_name)) +
  labs(title = "Arrestos en Los Ángeles", x = "Longitud", y = "Latitud")

# Convertir el gráfico ggplot en un gráfico interactivo con plotly
ggplotly(p, tooltip = "text")



##########################

datos <- la_arrest[, c("crime_category", "victim_gender", "premises_description", "weapon_description")]

#factoriza todas las variables
datos[] <- lapply(datos, factor)

#FRECUENCIA POR ITEMS
datos_transacciones <- as(datos, "transactions")
#ver frecuencia de aparicion de cada elemento
frec <- arules:: itemFrequency(x = datos_transacciones, type = "relative")
frec %>%
  sort(decreasing = TRUE)%>%
  head(8)

#tienen que filtrar por las variables que les interesen o quieran relacionar
#sacar lo de los codigos que genera asociacion 

soporte <- (1000)/ dim(datos)[1]
reglas <- apriori(data = datos,
                  parameter = list(support = soporte,
                                   confidence = 0.70,
                                   minlen = 3,
                                   # Se especifica que se creen reglas
                                   target = "rules"))

reglas <- subset(reglas, rhs %in% "victim_gender=Male" | rhs %in% "victim_gender=Female") #aca
#rhs %in% "victim_gender=Male=Female"
reglas <- arules::subset(reglas, subset=(size(rhs)==1))
reglas <- arules::sort(reglas, by="lift", decreasing=TRUE)
summary(reglas)
length(reglas)
reglas %>% 
  arules::inspect()


plot(reglas, method="graph", control=list(type="items"))
plot(reglas, measure = c("support", "confidence"), shading = "lift")
plot(reglas, method = "scatterplot", measure = c("support", "lift"), shading = "confidence", engine = "ggplot2") + 
  scale_color_gradient2(low = "red", mid = "gray90", high = "blue", midpoint = 0.5, limits = c(0, 1)) +
  labs(x = "Support", y = "Lift", color = "Confidence") + 
  theme_classic()

# Convierte las reglas en un dataframe
reglas_df <- as(reglas, "data.frame")

write.csv(reglas_df, file = "male_data.csv")



as(reglas, Class = "data.frame") %>%
  ggplot(aes(x = reorder(rules, lift), y = lift)) +
  geom_col() +
  #coord_cartesian(ylim = c(1.02, 1.038)) +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()+
  coord_flip()

summary(reglas)
















