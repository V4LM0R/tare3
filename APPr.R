# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Leer el archivo CSV con delimitador de punto y coma
datosd <- read.csv("datos.csv", sep = ";", stringsAsFactors = FALSE)

# Definir nombres de columnas
colnames(datosd) <- c(
  "COLEGIO", "COLEGIO_DEPA", "COLEGIO_ANIO_EGRESO", "ESPECIALIDAD",
  "DOMICILIO_DIST", "ANIO_NACIMIENTO", "NACIMIENTO_PAIS", "NACIMIENTO_DEPA",
  "SEXO", "CALIF_FINAL", "INGRESO", "MODO_INGRESO"
)

# Asegurarse de que los tipos de datos son correctos
datosd$COLEGIO <- as.character(datosd$COLEGIO)
datosd$COLEGIO_DEPA <- as.character(datosd$COLEGIO_DEPA)
datosd$COLEGIO_ANIO_EGRESO <- as.numeric(datosd$COLEGIO_ANIO_EGRESO)
datosd$ESPECIALIDAD <- as.character(datosd$ESPECIALIDAD)
datosd$DOMICILIO_DIST <- as.character(datosd$DOMICILIO_DIST)
datosd$ANIO_NACIMIENTO <- as.numeric(datosd$ANIO_NACIMIENTO)
datosd$NACIMIENTO_PAIS <- as.character(datosd$NACIMIENTO_PAIS)
datosd$NACIMIENTO_DEPA <- as.character(datosd$NACIMIENTO_DEPA)
datosd$SEXO <- as.character(datosd$SEXO)
datosd$CALIF_FINAL <- as.numeric(datosd$CALIF_FINAL)
datosd$INGRESO <- as.character(datosd$INGRESO)
datosd$MODO_INGRESO <- as.character(datosd$MODO_INGRESO)

# Crear variable binaria para ingreso
ingreso_binario <- ifelse(datosd$INGRESO == "SI", 1, 0)

# Calcular el total de ingresantes y no ingresantes
total_ingresantes <- sum(ingreso_binario, na.rm = TRUE)
total_no_ingresantes <- sum(ingreso_binario == 0, na.rm = TRUE)
n_total <- total_ingresantes + total_no_ingresantes

# Realizar prueba binomial
binomial_test <- binom.test(total_ingresantes, n_total, p = 0.5, alternative = "two.sided")

# Crear tabla de resultados de la prueba binomial
tabla_binomial <- data.frame(
  Categoria = c("Sí", "No", "Total"),
  N = c(total_ingresantes, total_no_ingresantes, n_total),
  Prop. observada = c(total_ingresantes / n_total, total_no_ingresantes / n_total, 1),
  Prop. de prueba = c(0.5, 0.5, NA),
  Significación exacta (bilateral) = c(
    format(round(binomial_test$p.value, 3), nsmall = 3),
    NA,
    NA
  )
)

# Mostrar la tabla
print(tabla_binomial)

# Calcular la variable EDAD si no está calculada
datosd$EDAD <- 2023 - datosd$ANIO_NACIMIENTO  # Asegúrate de que el año de nacimiento esté en el formato correcto

# Calcular las estadísticas y redondear los valores de edad
estadisticas <- datosd %>%
  group_by(INGRESO) %>%
  summarise(
    # Estadísticas de CALIF_FINAL
    Frecuencia_Calif = n(),
    Media_Calif = mean(CALIF_FINAL, na.rm = TRUE),
    Desviacion_Calif = sd(CALIF_FINAL, na.rm = TRUE),
    Max_Calif = max(CALIF_FINAL, na.rm = TRUE),
    Min_Calif = min(CALIF_FINAL, na.rm = TRUE),
    
    # Estadísticas de EDAD (redondeando media, máximo y mínimo)
    Frecuencia_Edad = n(),
    Media_Edad = round(mean(EDAD, na.rm = TRUE), 0),  # Redondear la media de EDAD
    Desviacion_Edad = sd(EDAD, na.rm = TRUE),
    Max_Edad = round(max(EDAD, na.rm = TRUE), 0),  # Redondear el máximo de EDAD
    Min_Edad = round(min(EDAD, na.rm = TRUE), 0)   # Redondear el mínimo de EDAD
  )

# Crear la tabla final con las estadísticas
tabla_final <- data.frame(
  Variable = c("CALIF_FINAL", "", "", "", "", "EDAD", "", "", "", ""),
  Indicador = c("Frecuencia", "Media", "Desviación est.", "Máximo", "Mínimo",
                "Frecuencia", "Media", "Desviación est.", "Máximo", "Mínimo"),
  No = c(estadisticas$Frecuencia_Calif[estadisticas$INGRESO == "NO"],
         estadisticas$Media_Calif[estadisticas$INGRESO == "NO"],
         estadisticas$Desviacion_Calif[estadisticas$INGRESO == "NO"],
         estadisticas$Max_Calif[estadisticas$INGRESO == "NO"],
         estadisticas$Min_Calif[estadisticas$INGRESO == "NO"],
         estadisticas$Frecuencia_Edad[estadisticas$INGRESO == "NO"],
         estadisticas$Media_Edad[estadisticas$INGRESO == "NO"],
         estadisticas$Desviacion_Edad[estadisticas$INGRESO == "NO"],
         estadisticas$Max_Edad[estadisticas$INGRESO == "NO"],
         estadisticas$Min_Edad[estadisticas$INGRESO == "NO"]),
  Sí = c(estadisticas$Frecuencia_Calif[estadisticas$INGRESO == "SI"],
         estadisticas$Media_Calif[estadisticas$INGRESO == "SI"],
         estadisticas$Desviacion_Calif[estadisticas$INGRESO == "SI"],
         estadisticas$Max_Calif[estadisticas$INGRESO == "SI"],
         estadisticas$Min_Calif[estadisticas$INGRESO == "SI"],
         estadisticas$Frecuencia_Edad[estadisticas$INGRESO == "SI"],
         estadisticas$Media_Edad[estadisticas$INGRESO == "SI"],
         estadisticas$Desviacion_Edad[estadisticas$INGRESO == "SI"],
         estadisticas$Max_Edad[estadisticas$INGRESO == "SI"],
         estadisticas$Min_Edad[estadisticas$INGRESO == "SI"])
)

# Formatear la tabla
library(kableExtra)
tabla_formateada <- tabla_final %>%
  kbl(align = "c", caption = "Estadísticas Descriptivas por Grupo (SÍ y NO)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  collapse_rows(columns = 1, valign = "middle")

# Mostrar la tabla
tabla_formateada


#3333

# Distribución de Calificaciones Finales
ggplot(datosd, aes(x = INGRESO, y = CALIF_FINAL, fill = INGRESO)) +
  geom_boxplot() +
  labs(title = "Distribución de Calificaciones Finales por Estado de Ingreso",
       x = "Ingreso", y = "Calificación Final") +
  theme_minimal()

#4444

# Tabla de frecuencia cruzada por especialidad e ingreso
tabla_especialidad_ingreso <- table(datosd$ESPECIALIDAD, datosd$INGRESO)
kable(tabla_especialidad_ingreso, format = "html", caption = "Frecuencia Cruzada de Especialidad e Ingreso") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Tabla de frecuencia cruzada por modo de ingreso e ingreso
tabla_modo_ingreso <- table(datosd$MODO_INGRESO, datosd$INGRESO)
kable(tabla_modo_ingreso, format = "html", caption = "Frecuencia Cruzada de Modo de Ingreso e Ingreso") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

#555
# Gráfico de barras por especialidad
ggplot(datosd, aes(x = ESPECIALIDAD, fill = INGRESO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Ingreso por Especialidad", x = "Especialidad", y = "Proporción") +
  theme_minimal()

# Gráfico de barras por modo de ingreso
ggplot(datosd, aes(x = MODO_INGRESO, fill = INGRESO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Ingreso por Modo de Ingreso", x = "Modo de Ingreso", y = "Proporción") +
  theme_minimal()


#666

# Convertir la variable INGRESO a binaria
datosd$INGRESO_BINARIO <- ifelse(datosd$INGRESO == "SI", 1, 0)

# Verificar la conversión
head(datosd)

# Modelos de regresión logística usando la variable binaria
model <- glm(INGRESO_BINARIO ~ CALIF_FINAL + ANIO_NACIMIENTO + SEXO + NACIMIENTO_DEPA, data = datosd, family = binomial)
summary(model)

# Predicción de probabilidades
datosd$prob_ingreso <- predict(model, type = "response")
datosd$pred_ingreso <- ifelse(datosd$prob_ingreso > 0.5, "SI", "NO")

# Visualización de las probabilidades
ggplot(datosd, aes(x = CALIF_FINAL, y = prob_ingreso, color = INGRESO)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Probabilidad de Ingreso en Función de la Calificación Final", x = "Calificación Final", y = "Probabilidad de Ingreso") +
  theme_minimal()

#7777


# Crear una tabla de datos para el gráfico
tabla_anio_egreso_ingreso_df <- as.data.frame(tabla_anio_egreso_ingreso)
colnames(tabla_anio_egreso_ingreso_df) <- c("COLEGIO_ANIO_EGRESO", "INGRESO", "FREQ")

# Gráfico de barras
ggplot(tabla_anio_egreso_ingreso_df, aes(x = COLEGIO_ANIO_EGRESO, y = FREQ, fill = INGRESO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencia de Ingreso por Año de Egreso", x = "Año de Egreso", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("NO" = "red", "SI" = "green"))


#888
# Gráfico de barras por departamento del colegio
ggplot(datosd, aes(x = COLEGIO_DEPA, fill = INGRESO)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de Ingreso por Departamento del Colegio", x = "Departamento del Colegio", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#999

# Crear un dataframe resumen para gráfico circular
resumen_pais <- datosd %>%
  group_by(NACIMIENTO_PAIS, INGRESO) %>%
  summarise(conteo = n()) %>%
  mutate(proporcion = conteo / sum(conteo))

# Gráfico circular por país de nacimiento
ggplot(resumen_pais, aes(x = "", y = proporcion, fill = INGRESO)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  facet_wrap(~ NACIMIENTO_PAIS) +
  labs(title = "Distribución de Ingreso por País de Nacimiento", x = NULL, y = NULL) +
  theme_minimal()