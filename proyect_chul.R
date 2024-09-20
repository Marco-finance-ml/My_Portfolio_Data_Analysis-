install.packages("tidyverse")
library(tidyverse)
install.packages("rmarkdown")
archivo <-file.choose()
## metadata
colnames(dailyActivity_merged)
dim(dailyActivity_merged)
head(dailyActivity_merged)
## Analisis
str(dailyActivity_merged)
## Realizamos limpieza de datos. Cambiamos los tipos de datos que evaluamos deban ser corregidos.
dailyActivity_merged$ActivityDate <- as.Date(dailyActivity_merged$ActivityDate, format="%m/%d/%Y")
dailyActivity_merged$Id <- as.character(dailyActivity_merged$Id)
str(dailyActivity_merged)
duplicated(dailyActivity_merge)
## Datos estadísticos
summary(dailyActivity_merged)
## Segmentación

## Crear la columna de categorías usando TotalDistance como referencia
dailyActivity_merged$Categoría <- ifelse(
  dailyActivity_merged$TotalDistance > 7, "Vigorosos",
  ifelse(dailyActivity_merged$TotalDistance >= 5, "Normales", "Flojos")
)

View(dailyActivity_merged)
##A continuación vamos a realizar gráficas que complementaron nuestro análisis
install.packages("ggplot2")
library(ggplot2)

ggplot(dailyActivity_merged, aes(x = Categoría)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "skyblue", color = "black") +
  geom_text(
    aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 1)),
    stat = "count",
    vjust = -0.5  # Ajusta la posición vertical del texto
  ) +
  labs(
    x = "Categoría",
    y = "Porcentaje",
    title = "Distribución en función a la Distancia total por Categoría"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_minimal()
## Analizamos correlación entre Total de Pasos y Calorias Gastadas.
ggplot(dailyActivity_merged, aes(x = TotalSteps , y = Calories)) +
  geom_point(color = "blue", size = 1.5) +  # Agrega puntos al gráfico
  geom_smooth(method = "lm", color = "red") +  # Añade una línea de regresión lineal
  labs(
    x = "Total de Pasos",
    y = "Calorías",
    title = "Correlación entre Pasos Totales y Calorías"
  ) +
  theme_minimal()
## Analizamos correlación entre Total de Pasos y Calorias Gastadas segmentado por categorias

ggplot(dailyActivity_merged, aes(x = TotalSteps, y = Calories, color = Categoría)) +
  geom_point(size = 1.5) +  # Agrega puntos al gráfico con colores diferenciados
  geom_smooth(method = "lm", aes(group = Categoría), se = FALSE) +  # Línea de regresión para cada categoría
  labs(
    x = "Total de Pasos",
    y = "Calorías",
    title = "Correlación entre Pasos Totales y Calorías por Categoría"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Vigorosos" = "blue", "Normales" = "green", "Flojos" = "red"))
##RECOMENDACIONES 
## *Tomar distintas distintas campañas en función a distintas categorías
## *A la categoría flojo se le podría poner incentivos para mayor uso como
## recordatorios, mensajes de aliento en función a logros, información sobre riesgos del sedentarismo, etc
## *Caso similar se da con el caso de la categoría normal. Para ellos el marketing seria de fidelización
## Recomendariamos realizar rankings de competición, información de progreso respecto a factores como peso, glucosa para generar incentivos.
##Para el caso de vigorosos, se podría realizar el marketing mediante sponsor a los usuarios más destacado, siendo elementos de publicidad.