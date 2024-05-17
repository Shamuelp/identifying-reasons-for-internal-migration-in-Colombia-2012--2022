#Script 2012----

#P6040 ¿cuántos años cumplidos tiene...? (si es menor de 1 año, escriba 00) 
#P6074 ¿_______________ siempre ha vivido aquí en este municipio? 
#P6075 ¿cuántos años continuos hace que vive _______________ aquí en este municipio? 
#P6076 Antes de venir a este municipio_______________ vivía en 
#P6077 _____ vivía en 
#P5739 ¿cuál fue la razón principal para cambiar la residencia al municipio actual?

#Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2012 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2012)

#Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2012 <- Caracteristicas_y_composicion_del_hogar_2012 %>%
  select(Directorio, P6040, P6074, P6075, P6077, P5739)

#Limpiamos los NA
Data_Base2012$P6075 <- ifelse(is.na(Data_Base2012$P6075), 0, Data_Base2012$P6075)
Data_Base2012$P6077 <- ifelse(is.na(Data_Base2012$P6077), 0, Data_Base2012$P6077)
Data_Base2012$P5739 <- ifelse(is.na(Data_Base2012$P5739), 0, Data_Base2012$P5739)

#Visualizamos min y max de edades
minimo <- min(Data_Base2012$P6040, na.rm = FALSE)
maximo <- max(Data_Base2012$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

#Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2012$rango_edad <- cut(Data_Base2012$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2012 <- table(Data_Base2012$rango_edad)

#Observamos la distribución de la poblacion en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2012)

#Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2012)
rm(Caracteristicas_y_composicion_del_hogar_2012)

#Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2012$P5739 <- recode(Data_Base2012$P5739,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              `5` = 5,
                              `6` = 6,
                              `7` = 7,
                              `8` = 8,
                              `9` = 11)

#Agrupación de los datos respecto a la columna P5739 con los números del 1 al 11 y rango de edad
Data_2012 <- Data_Base2012 %>%
  group_by(rango_edad, P5739) %>%
  summarise(count = n())
Data_2012 <- Data_2012 %>%
  filter(P5739 != 0)

#Ordenar los datos por el rango de edad y el número
Data_2012 <- Data_2012[order(Data_2012$rango_edad, Data_2012$P5739), ]
rm(Data_Base2012)
rm(i)

#Calcular el total de personas por rango de edad
total_por_edad <- Data_2012 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2012 <- left_join(Data_2012, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2012 <- Data_2012 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)

#Script 2014----

#P6040 ¿cuántos años cumplidos tiene...? 
#P6074 ¿_______________ siempre ha vivido aquí en este municipio? 
#P767 ¿cuántos años continuos hace que vive ___ aquí en este municipio? 
#P6076  Antes de venir a este municipio_______________ vivía en 
#P6077 _____ vivía en 
#P6096 ¿cuál fue la razón principal para cambiar la residencia al municipio actual

#Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2014 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2014)

#Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2014 <- Caracteristicas_y_composicion_del_hogar_2014 %>%
  select(DIRECTORIO, P6040, P6074, P767, P6077, P6096)

#Limpiamos los NA
Data_Base2014$P767 <- ifelse(is.na(Data_Base2014$P767), 0, Data_Base2014$P767)
Data_Base2014$P6077 <- ifelse(is.na(Data_Base2014$P6077), 0, Data_Base2014$P6077)
Data_Base2014$P6096 <- ifelse(is.na(Data_Base2014$P6096), 0, Data_Base2014$P6096)

#Visualizamos min y max de edades
minimo <- min(Data_Base2014$P6040, na.rm = FALSE)
maximo <- max(Data_Base2014$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

#Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2014$rango_edad <- cut(Data_Base2014$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2014 <- table(Data_Base2014$rango_edad)

#Observamos la distribución de la poblacion en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2014)

#Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2014)
rm(Caracteristicas_y_composicion_del_hogar_2014)

#Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2014$P6096 <- recode(Data_Base2014$P6096,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              `5` = 5,
                              `6` = 6,
                              `7` = 7,
                              `8` = 8,
                              `9` = 11)

# Agrupación de los datos respecto a la columna P6096 con los números del 1 al 11 y rango de edad
Data_2014 <- Data_Base2014 %>%
  group_by(rango_edad, P6096) %>%
  summarise(count = n())
Data_2014 <- Data_2014 %>%
  filter(P6096 != 0)

# Ordenar los datos por el rango de edad y el número
Data_2014 <- Data_2014[order(Data_2014$rango_edad, Data_2014$P6096), ]
rm(Data_Base2014)
rm(i)

#Calcular el total de personas por rango de edad
total_por_edad <- Data_2014 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2014 <- left_join(Data_2014, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2014 <- Data_2014 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)

#Script 2016----

#P6040 ¿cuántos años cumplidos tiene...? 
#P6074 ¿_______________ siempre ha vivido aquí en este municipio? 
#P767 ¿cuántos años continuos hace que vive ___ aquí en este municipio? 
#P6076 Antes de venir a este municipio_______________ vivía en 
#P6077_____ vivía en 
#P6096 ¿cuál fue la razón principal para cambiar la residencia al municipio actual?
  
#Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2016 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2016)

#Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2016 <- Caracteristicas_y_composicion_del_hogar_2016 %>%
  select(DIRECTORIO, P6040, P6074, P767, P6077, P6096)

#Limpiamos los NA
Data_Base2016$P767 <- ifelse(is.na(Data_Base2016$P767), 0, Data_Base2016$P767)
Data_Base2016$P6077 <- ifelse(is.na(Data_Base2016$P6077), 0, Data_Base2016$P6077)
Data_Base2016$P6096 <- ifelse(is.na(Data_Base2016$P6096), 0, Data_Base2016$P6096)

#Visualizamos min y max de edades
minimo <- min(Data_Base2016$P6040, na.rm = FALSE)
maximo <- max(Data_Base2016$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

#Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2016$rango_edad <- cut(Data_Base2016$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2016 <- table(Data_Base2016$rango_edad)

#Observamos la distribución de la poblacion en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2016)

#Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2016)
rm(Caracteristicas_y_composicion_del_hogar_2016)

#Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2016$P6096 <- recode(Data_Base2016$P6096,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              `5` = 5,
                              `6` = 6,
                              `7` = 7,
                              `8` = 8,
                              `9` = 9,
                              `10` = 11)

# Agrupación de los datos respecto a la columna P6096 con los números del 1 al 11 y rango de edad
Data_2016 <- Data_Base2016 %>%
  group_by(rango_edad, P6096) %>%
  summarise(count = n())
Data_2016 <- Data_2016 %>%
  filter(P6096 != 0)
# Ordenar los datos por el rango de edad y el número
Data_2016 <- Data_2016[order(Data_2016$rango_edad, Data_2016$P6096), ]
rm(Data_Base2016)
rm(i)

#Calcular el total de personas por rango de edad
total_por_edad <- Data_2016 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2016 <- left_join(Data_2016, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2016 <- Data_2016 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)

#Script 2018----

#P6040 ¿cuántos años cumplidos tiene...? 
#P6074 ¿_______________ siempre ha vivido aquí en este municipio? 
#P767 ¿cuántos años continuos hace que vive ___ aquí en este municipio? 
#P6076 Antes de venir a este municipio_______________ vivía en 
#P6077_____ vivía en 
#P6096 ¿cuál fue la razón principal para cambiar la residencia al municipio actual?
  
#Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2018 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2018)

#Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2018 <- Caracteristicas_y_composicion_del_hogar_2018 %>%
  select(DIRECTORIO, P6040, P6074, P767, P6077, P6096)

#Limpiamos los NA
Data_Base2018$P767 <- ifelse(is.na(Data_Base2018$P767), 0, Data_Base2018$P767)
Data_Base2018$P6077 <- ifelse(is.na(Data_Base2018$P6077), 0, Data_Base2018$P6077)
Data_Base2018$P6096 <- ifelse(is.na(Data_Base2018$P6096), 0, Data_Base2018$P6096)

#Visualizamos min y max de edades
minimo <- min(Data_Base2018$P6040, na.rm = FALSE)
maximo <- max(Data_Base2018$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

#Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2018$rango_edad <- cut(Data_Base2018$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2018 <- table(Data_Base2018$rango_edad)

#Observamos la distribución de la poblacion en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2018)

#Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2018)
rm(Caracteristicas_y_composicion_del_hogar_2018)

#Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2018$P6096 <- recode(Data_Base2018$P6096,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              `5` = 5,
                              `6` = 6,
                              `7` = 7,
                              `8` = 8,
                              `9` = 9,
                              `10` = 11)

# Agrupación de los datos respecto a la columna P6096 con los números del 1 al 11 y rango de edad
Data_2018 <- Data_Base2018 %>%
  group_by(rango_edad, P6096) %>%
  summarise(count = n())
Data_2018 <- Data_2018 %>%
  filter(P6096 != 0)
# Ordenar los datos por el rango de edad y el número
Data_2018 <- Data_2018[order(Data_2018$rango_edad, Data_2018$P6096), ]
rm(Data_Base2018)
rm(i)

#Calcular el total de personas por rango de edad
total_por_edad <- Data_2018 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2018 <- left_join(Data_2018, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2018 <- Data_2018 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)

#Script 2020----

#P6040 ¿cuántos años cumplidos tiene...?
#P6074 ¿_______________ siempre ha vivido aquí en este municipio
#P767 ¿cuántos años continuos hace que vive ___ aquí en este municipio?
#P6076 Antes de venir a este municipio_______________ vivía en
#P6077 _____ vivía en
#P6096 ¿cuál fue la razón principal para cambiar la residencia al municipio actual?
  
# Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2020 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2020)

# Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2020 <- Caracteristicas_y_composicion_del_hogar_2020 %>%
  select(DIRECTORIO, P6040, P6074, P767, P6077, P6096)

# Limpiamos los NA
Data_Base2020$P767 <- ifelse(is.na(Data_Base2020$P767), 0, Data_Base2020$P767)
Data_Base2020$P6077 <- ifelse(is.na(Data_Base2020$P6077), 0, Data_Base2020$P6077)
Data_Base2020$P6096 <- ifelse(is.na(Data_Base2020$P6096), 0, Data_Base2020$P6096)

# Visualizamos min y max de edades
minimo <- min(Data_Base2020$P6040, na.rm = FALSE)
maximo <- max(Data_Base2020$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

# Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2020$rango_edad <- cut(Data_Base2020$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2020 <- table(Data_Base2020$rango_edad)

# Observamos la distribución de la población en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2020)

# Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2020)
rm(Caracteristicas_y_composicion_del_hogar_2020)

# Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2020$P6096 <- recode(Data_Base2020$P6096,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              `5` = 5,
                              `6` = 6,
                              `7` = 7,
                              `8` = 8,
                              `9` = 9,
                              `10` = 10,
                              `12` = 11)

# Agrupación de los datos respecto a la columna P6096 con los números del 1 al 11 y rango de edad
Data_2020 <- Data_Base2020 %>%
  group_by(rango_edad, P6096) %>%
  summarise(count = n())
Data_2020 <- Data_2020 %>%
  filter(P6096 != 0)
# Ordenar los datos por el rango de edad y el número
Data_2020 <- Data_2020[order(Data_2020$rango_edad, Data_2020$P6096), ]
rm(Data_Base2020)
rm(i)

#Calcular el total de personas por rango de edad
total_por_edad <- Data_2020 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2020 <- left_join(Data_2020, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2020 <- Data_2020 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)

#Script 2022----

#P6040 ¿cuántos años cumplidos tiene...?
#P6074 ¿Siempre ha vivido aquí en este municipio?
#P755 ¿Dónde vivía ..., hace 5 años?
#P754 El lugar donde vivía ... hace 5 años era:
#P753 ¿Dónde vivía ..., hace 12 meses?
#P752 El lugar donde vivía ... hace 12 meses era:
#P1662 ¿Cuál fue el principal motivo por el que ... Cambió el lugar donde residia hace 12 meses?
  

# Traemos los datos SPSS
library(haven)
Caracteristicas_y_composicion_del_hogar_2022 <- read_sav("file.sav")
View(Caracteristicas_y_composicion_del_hogar_2022)

# Seleccionamos solo las variables que necesitamos
library(dplyr)
Data_Base2022 <- Caracteristicas_y_composicion_del_hogar_2022 %>%
  select(DIRECTORIO, P6040, P6074, P755, P753, P1662)

# Limpiamos los NA
Data_Base2022$P755 <- ifelse(is.na(Data_Base2022$P755), 0, Data_Base2022$P755)
Data_Base2022$P753 <- ifelse(is.na(Data_Base2022$P753), 0, Data_Base2022$P753)
Data_Base2022$P1662 <- ifelse(is.na(Data_Base2022$P1662), 0, Data_Base2022$P1662)

# Visualizamos min y max de edades
minimo <- min(Data_Base2022$P6040, na.rm = FALSE)
maximo <- max(Data_Base2022$P6040, na.rm = FALSE)
rm(minimo)
rm(maximo)

# Establecemos los rangos de edad con los que trabajaremos
limites_edad <- c(0, 20, 30, 40, 50, 60, 70, Inf)
etiquetas_edad <- c("<20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", ">70")
Data_Base2022$rango_edad <- cut(Data_Base2022$P6040, 
                                breaks = limites_edad,
                                labels = etiquetas_edad,
                                include.lowest = TRUE)
poblacion_por_rango2022 <- table(Data_Base2022$rango_edad)

# Observamos la distribución de la población en esos rangos de edades 
print("Pirámide Poblacional:")
print(poblacion_por_rango2022)

# Borramos objetos que no necesitamos
rm(etiquetas_edad)
rm(limites_edad)
rm(poblacion_por_rango2022)
rm(Caracteristicas_y_composicion_del_hogar_2022)

# Ahora vamos a organizar los motivos de movilidad ajustados a nuestro nueva categorización

Data_Base2022$P1662 <- recode(Data_Base2022$P1662,
                              `1` = 1,
                              `6` = 2,
                              `4` = 3,
                              `2` = 4,
                              `7` = 5,
                              `3` = 6,
                              `10` = 10,
                              `11` = 11)

# Agrupación de los datos respecto a la columna P1662 con los números del 1 al 11 y rango de edad
Data_2022 <- Data_Base2022 %>%
  group_by(rango_edad, P1662) %>%
  summarise(count = n())
Data_2022 <- Data_2022 %>%
  filter(P1662 != 0)
Data_2022 <- Data_2022 %>%
  filter(P1662 != 8)
# Ordenar los datos por el rango de edad y el número
Data_2022 <- Data_2022[order(Data_2022$rango_edad, Data_2022$P1662), ]
rm(Data_Base2022)
rm(i)


#Calcular el total de personas por rango de edad
total_por_edad <- Data_2022 %>%
  group_by(rango_edad) %>%
  summarise(total_personas = sum(count))

#Unir el total de personas por rango de edad al dataframe original
Data_2022 <- left_join(Data_2022, total_por_edad, by = "rango_edad")

#Calcular el porcentaje que cada valor de P5739 representa por rango de edad
Data_2022 <- Data_2022 %>%
  mutate(percentage = (count / total_personas) * 100)
rm(total_por_edad)
#Downloads----

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

#Ejecutando las siguientes lineas se descargan los datos en formato de excel
write.xlsx(Data_2012, file = "path.xlsx", rowNames = FALSE)
write.xlsx(Data_2014, file = "path.xlsx", rowNames = FALSE)
write.xlsx(Data_2016, file = "path.xlsx", rowNames = FALSE)
write.xlsx(Data_2018, file = "path.xlsx", rowNames = FALSE)
write.xlsx(Data_2020, file = "path.xlsx", rowNames = FALSE)
write.xlsx(Data_2022, file = "path.xlsx", rowNames = FALSE)

