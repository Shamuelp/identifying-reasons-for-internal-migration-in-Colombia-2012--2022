#Let´s Plot---- 
#Aqui puedes seleccionar el año y el rango de edad que deseas ver:

año <- "2018"      #Entre comillas eliges el año
r_edad <- ">70"    #Entre comillas eliges el rango de edad dejando espacio entre el (-) y los numeros

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
ggplot(data, aes(x = factor(P5739), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = paste("Porcentajes de Razones de Migración para el Rango de Edad", r_edad),
    x = "Razones de Migración",
    y = "Porcentaje"
  ) +
  theme_minimal()