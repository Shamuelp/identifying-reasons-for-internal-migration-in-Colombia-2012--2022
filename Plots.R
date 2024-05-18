#Lets Plot----

#Aqui puedes seleccionar el año y el rango de edad que deseas ver:

año <- "2020"      #Entre comillas elijes el año
r_edad <- "20 - 30"    #Entre comillas eliges el rango de edad

#Plot Code----
if (año == "2012") {
  data <- Data_2012
} else if (año == "2014") {
  data <- Data_2014
} else if (año == "2016") {
  data <- Data_2016
} else if (año == "2018") {
  data <- Data_2018
} else if (año == "2020") {
  data <- Data_2020
} else if (año == "2022") {
  data <- Data_2022
} else {
  stop("Año no válido. Selecciona uno de los siguientes: 2012, 2014, 2016, 2018, 2020, 2022.")
}
#Filtrar los datos para el rango de edad especificado
data <- subset(Data_2012, rango_edad == r_edad)
#Plot
library(ggplot2)
ggplot(data, aes(x = factor(P5739), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = paste("Porcentajes de Razones de Migración para el Rango de Edad", r_edad),
    x = "Razones de Migración",
    y = "Porcentaje"
  ) +
  theme_minimal()
#Mostrar todos los años juntos----

# Seleccionamos las columnas necesarias y las renombramos para que sean consistentes
Data_2012_sub <- Data_2012[, c("rango_edad", "P5739", "count")]
colnames(Data_2012_sub) <- c("rango_edad", "Var", "count")

Data_2014_sub <- Data_2014[, c("rango_edad", "P6096", "count")]
colnames(Data_2014_sub) <- c("rango_edad", "Var", "count")

Data_2016_sub <- Data_2016[, c("rango_edad", "P6096", "count")]
colnames(Data_2016_sub) <- c("rango_edad", "Var", "count")

Data_2018_sub <- Data_2018[, c("rango_edad", "P6096", "count")]
colnames(Data_2018_sub) <- c("rango_edad", "Var", "count")

Data_2020_sub <- Data_2020[, c("rango_edad", "P6096", "count")]
colnames(Data_2020_sub) <- c("rango_edad", "Var", "count")

Data_2022_sub <- Data_2022[, c("rango_edad", "P1662", "count")]
colnames(Data_2022_sub) <- c("rango_edad", "Var", "count")

# Unimos todos los dataframes en uno solo
combined_data <- rbind(Data_2012_sub, Data_2014_sub, Data_2016_sub, Data_2018_sub, Data_2020_sub, Data_2022_sub)

# Sumamos la columna count agrupando por rango_edad y Var
final_data <- aggregate(count ~ rango_edad + Var, data = combined_data, FUN = sum)

# Renombramos la columna 'count' a 'sum'
colnames(final_data)[colnames(final_data) == "count"] <- "sum"

grupo_peso <- final_data %>%
  group_by(rango_edad, Var) %>%
  summarise(Peso_Total = sum(sum, na.rm = TRUE)) %>%
  ungroup()

# Calcula la proporción ponderada dentro de cada rango de edad
grupo_peso <- grupo_peso %>%
  group_by(rango_edad) %>%
  mutate((Proporcion_Ponderada = Peso_Total / sum(Peso_Total, na.rm = TRUE))*100) %>%
  ungroup()
colnames(grupo_peso)[colnames(grupo_peso) == "*..."] <- "porcentaje"

#Plot General 10 años----

r_edad <- "20 - 30"    #Entre comillas eliges el rango de edad


datag <- subset(grupo_peso, rango_edad == r_edad)
#Plot
library(ggplot2)
ggplot(datag, aes(x = factor(Var), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = paste("Porcentajes de Razones de Migración para el Rango de Edad", r_edad),
    x = "Razones de Migración",
    y = "Porcentaje"
  ) +
  theme_minimal()
