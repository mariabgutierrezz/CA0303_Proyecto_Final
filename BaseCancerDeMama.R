install.packages("univariateML")


library(dplyr)
library(tidyr)
library(ggplot2)
library(univariateML)


# Replace "your_file.csv" with the actual file path
datos <- read.csv("/Users/esaurm27/Desktop/SEERBreastCancerDataset.csv")

# Now 'data' contains the contents of the CSV file as a data frame

# Suponiendo que 'data' es tu data frame
# Suponiendo que 'datos' es tu data frame y 'Race' es el nombre de la columna "raza"
count_black <- sum(grepl("black", datos$Race, ignore.case = TRUE))

count_white <- sum(grepl("White", datos$Race, ignore.case = TRUE))

# 'count_black' contendrá la cantidad de observaciones que tienen "black" en la columna "Race"

#Eliminar columna llamada "X" de NA's
datos <- datos[, -which(names(datos) == "X")]

#Eliminar observaciones que tengan NA en algún sitio
datos <- na.omit(datos)

#Eliminar columnas que no se van a necesitar
columns_to_drop <- c(3, 4, 5, 6, 8, 10, 11, 12,13)
datos <- datos[, -columns_to_drop]

#Eliminar texto innecesario de la columna Grade
datos$Grade <- sub(".*;", "", datos$Grade)

#
youngest_age <- min(datos$Age)

oldest_age <- max(datos$Age)

#Chunkito
edades_breaks <- c(30, 40, 50, 60, 70)
edades_labels <- c("30-39", "40-49", "50-59", "60-69")

# Agregar una columna al DataFrame con las etiquetas de grupo
datos$grupo_edades <- cut(datos$Age, breaks = edades_breaks, labels = edades_labels, right = FALSE)


# Separar el DataFrame en subconjuntos basados en las etiquetas de grupo
df_30_39 <- datos[datos$grupo_edades == "30-39", ]
df_40_49 <- datos[datos$grupo_edades == "40-49", ]
df_50_59 <- datos[datos$grupo_edades == "50-59", ]
df_60_69 <- datos[datos$grupo_edades == "60-69", ]

# Supongamos que tienes un DataFrame llamado 'datos' con las columnas "grupo", "raza" y "tamaño"

#Columna con valores numericos para el estado
datos$Status_Code <- ifelse(datos$Status == "Alive", 1, 0)

#Columna con valores numericos para el grado del cancer
datos$Grade_Code <- ifelse(datos$Grade == " Grade I", 1,
                          ifelse(datos$Grade == " Grade II", 2,
                                 ifelse(datos$Grade == " Grade III", 3,
                                        ifelse(datos$Grade == " Grade IV", 4, NA))))



#Creación de Tabla 

# Calcular el promedio de "Tumor.Size" por grupo y raza
resultado <- datos %>%
  group_by(grupo_edades, Race) %>%
  summarise(Promedio_Tumor_Size = mean(Tumor.Size))


############################
# Usar la función pivot_wider para convertir los resultados en una tabla
tabla_Promedios <- resultado %>%
  pivot_wider(names_from = Race, values_from = Promedio_Tumor_Size)

# Si algunas combinaciones grupo-raza no tenían datos, puedes llenar los NA con 0
tabla_Promedios [is.na(tabla_Promedios)] <- 0

# Visualizar la tabla final
print(tabla_Promedios)

## Gráfico
# Crear un gráfico de barras
grafico <- ggplot(datos, aes(x = grupo_edades, y = Survival.Months, fill = Race)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Promedio de Sobrevivencia por Edad y Raza",
       x = "Edad",
       y = "Promedio de Sobrevivencia") +
  theme_minimal() +
  scale_fill_discrete(name = "Raza")

# Mostrar el gráfico
print(grafico)


grafico1 <- ggplot(datos, aes(x = Grade, y = Survival.Months, fill = Race)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Promedio de Sobrevivencia por Grado y Raza",
       x = "Grado",
       y = "Promedio de Sobrevivencia") +
  theme_minimal() +
  scale_fill_discrete(name = "Raza")

# Mostrar el gráfico
print(grafico1)

#############################################################################
tabla_conteo <- table(datos$grupo_edades, datos$Race)

# Convierte la tabla en un dataframe
tabla_conteo_df <- as.data.frame.matrix(tabla_conteo)

# Cambia el nombre de las filas y columnas si es necesario
# Por ejemplo, si deseas que "grupo" sea el nombre de las filas
rownames(tabla_conteo_df) <- rownames(tabla_conteo_df)
colnames(tabla_conteo_df) <- colnames(tabla_conteo_df)

# Visualizar la tabla de conteo
print(tabla_conteo_df)
#####
datos_filtrados <- datos[datos$Race == "Black", ]

# Cuenta la cantidad de observaciones para cada valor único de la columna "grupo"
tabla_conteo <- table(datos_filtrados$grupo)

# Convierte la tabla en un dataframe
tabla_conteo_df <- as.data.frame.table(tabla_conteo)

# Renombra las columnas
colnames(tabla_conteo_df) <- c("Grupo", "Cantidad de Observaciones")

# Visualizar la tabla de conteo para "raza" = "negra"
print(tabla_conteo_df)

resultado <- datos %>%
  group_by(Race, Grade) %>%
  summarise(Min = min(Tumor.Size),
            Max = max(Tumor.Size),
            Var = var(Tumor.Size),
            Promedio = mean(Tumor.Size),
            .groups = "drop")
#############################################
resultado <- datos %>%
  group_by(Race, grupo_edades) %>%

resultados <- datos %>%
  group_by(raza, grupo_de_edad) %>%
  summarize(Muertes = sum(estado == "dead"), Sobrevivientes = sum(estado == "alive"))
################################################

data_grado <- datos %>%
  group_by(Race, Grade) %>%
  count()

data_grado <- data_grado %>%
  group_by(Race) %>%
  mutate(Porcentaje = n / sum(n) * 100)
###################################################

g1<- ggplot(data_grado, aes(x = Grade, y = Porcentaje, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Pacientes por Grado del Cáncer y Raza",
       x = "Grado",
       y = "Porcentaje") +
  theme_minimal()+ scale_fill_manual(values = c("White" = "#800080", "Black" = "blue", "Other (American Indian/AK Native, Asian/Pacific Islander)" = "lightblue"))
print(g1)

##################################################################
g1 <- ggplot(datos, aes(x = Tumor.Size, fill = Race)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribución del Tamaño del Tumor por Raza",
       x = "Tamaño del Tumor",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("Black" = "#800080", "Other" = "blue", "White" = "lightblue"))
# Visualizar el histograma
print(g1)

##################################################################
g1 <- ggplot(datos, aes(x = Tumor.Size, fill = Race)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribución del Tamaño del Tumor por Raza",
       x = "Tamaño del Tumor",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("Black" = "#800080", "Other" = "blue", "White" = "lightblue")) +
  facet_wrap(~Race, scales = "free_y")  # Crea un panel para cada raza

# Visualizar los histogramas
print(g1)

##################################################################
g1 <- ggplot(datos, aes(x = Tumor.Size, fill = Race)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribución del Tamaño del Tumor por Raza",
       x = "Tamaño del Tumor",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("Black" = "#800080", "Other" = "gray", "White" = "lightblue")) +
  facet_wrap(~Race, scales = "free_y")  # Crea un panel para cada raza

# Visualizar los histogramas
print(g1)
##################################################################
# Calcular la tabla de frecuencias
tabla_frecuencias <- table(datos$grupo_edades, datos$Race)

# Calcular el total de observaciones
total_observaciones <- sum(tabla_frecuencias)

# Calcular porcentajes
porcentajes <- (tabla_frecuencias / total_observaciones) * 100

# Crear un nuevo DataFrame con los porcentajes
tabla_porcentajes1 <- as.data.frame(porcentajes)

# Nombrar las filas y columnas
rownames(tabla_porcentajes) <- rownames(tabla_frecuencias)
colnames(tabla_porcentajes) <- colnames(tabla_frecuencias)

# Mostrar la tabla de porcentajes
print(tabla_porcentajes)


resultado1 <- datos %>%
  group_by(Race, grupo_edades) %>%
  summarise(Min = min(Survival.Months),
            Max = max(Survival.Months),
            Var = var(Survival.Months),
            Promedio = mean(Survival.Months),
            .groups = "drop")
#######################################################

######################################################### Aplicación de la Metodología. 
# Fit a normal distribution to the data
fit <- fitdist(datos$Tumor.Size, "pois")
# Print the summary of the fitted distribution
print(fit)
# Plot the histogram of the data and the fitted distribution
plot(fit)
######################################################### Aplicación de la Metodología. 
# Fit a normal distribution to the data
fit <- fitdist(datos$Survival.Months, "norm")
# Print the summary of the fitted distribution
print(fit)
# Plot the histogram of the data and the fitted distribution
plot(fit)
############################################################
r<- cdfcompcens(list(datos$Tumor.Size, datos$Tumor.Size), legendtext = c("lognormal", "loglogistic"))
print(r)
##############################################################
fit <- fitdist(datos$Status_Code, "binom")
# Print the summary of the fitted distribution
print(fit)
# Plot the histogram of the data and the fitted distribution
plot(fit)
##############################################################
dist_tumor_size<-model_select(datos$Tumor.Size, which = "BIC")
dist_status<- model_select(datos$Status_Code, which = "BIC")
dist_survival_months<- model_select(datos$Survival.Months, which = "BIC")
dist_grade<- model_select(datos$Grade_Code, which = "BIC")
dist_age<- model_select(datos$Age, which = "BIC")

##############################################################
alpha_previa <- 2.71186 # Shape parameter
beta_previa <- 0.04057  # Scale parameter

# Plot using ggplot2
ggplot(data = datos$Tumor.Size, aes(x = x)) +
  stat_function(
    fun = pllogis, # log-logistic distribution function
    args = list(shape = alpha_previa, scale = beta_previa),
    color = "blue", size = 2
  ) +
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  theme_minimal()

x<- datos$Tumor.Size
y <- plogis(log(x / beta_previa) / alpha_previa)

# Create a data frame with x and y values
df <- data.frame(x = x, y = y)

# Plot using ggplot2
ggplot(data = df, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 2) + xlab("Tamaño del Tumor")
  ylab("Cumulative Probability") +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_minimal()
  
  
  
  ########
  
  
  
  # Gráfico de dispersión facetado por Raza
 g2<- ggplot(datos, aes(x = factor(Grade), y = Age)) +
    geom_point() +
    facet_wrap(~Race) +
    labs(title = "Gráfico de Dispersión de Edad de Diagnóstico por Grado (Facetado por Raza)")
  
  g2
  #####
  # Gráfico de líneas por Raza
  
  # Gráfico de puntos por raza
  g2<- ggplot(data_grado, aes(x = factor(Grade), y = Porcentaje, color = Race)) +
    geom_point() +
    labs(title = "Gráfico de Puntos de Porcentaje por Grado (Facetado por Raza)")
  g2
