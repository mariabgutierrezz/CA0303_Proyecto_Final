# Paquetes
library(dplyr)
library(tidyverse)
library(ggplot2)
library(univariateML)
library(gamlss)
# Lectura de la base de datos
datos <- read.csv("/Users/esaurm27/Desktop/SEERBreastCancerDataset.csv")

#Limpieza de la base de datos:

###Se elemina columna con valores vacíos
datos <- datos[, -which(names(datos) == 'X')]

###Se eliminan las observaciones que tienen valores NA
datos <- na.omit(datos)

###Se eliminan las columnas de las variables que no son parte del estudio
columns_to_drop <- c(3, 4, 5, 6, 8, 10, 11, 12,13)
datos <- datos[, -columns_to_drop]

###Se elimina el texto innecesario de la columna Grade
datos$Grade <- sub(".*;", "", datos$Grade)

### Se modifica el nombre de las observaciones de la variable raza, para facilitar la manipulacion de los datos
datos <- datos %>%
    mutate(Race = ifelse(Race == "Other (American Indian/AK Native, Asian/Pacific Islander)", "Otra", 
                              ifelse(Race == "Black", "Negra", 
                                     ifelse(Race == "White", "Blanca", Race))))

#Se modifica el nombre de las observaciones de la variable Grado

datos <- datos %>%
    mutate(Grade = ifelse(Grade == " Grade I", "I", 
                          ifelse(Grade == " Grade II", "II", 
                                 ifelse(Grade == " Grade III", "III", 
                                        ifelse(Grade == " Grade IV", "IV", Grade)
                                        )
                                 )
                          )
    )


           
#Cantidad de observaciones por raza

count_black <- sum(grepl("Negra", datos$Raza, ignore.case = TRUE))

count_white <- sum(grepl("Blanca", datos$Raza, ignore.case = TRUE))

count_other <- sum(grepl("Otra", datos$Raza, ignore.case = TRUE))


# Rango de las edades de diagnóstico en estudio
youngest_age <- min(datos$Age)
oldest_age <- max(datos$Age)

#Se agrupan los datos por edades:
edades_breaks <- c(30, 40, 50, 60, 70)
edades_labels <- c("30-39", "40-49", "50-59", "60-69")

### Se agrega una columna al DataFrame con las etiquetas de grupo
datos$grupo_edades <- cut(datos$Age, breaks = edades_breaks, labels = edades_labels, right = FALSE)

###Separar el DataFrame en subconjuntos con respecto al grupo de edad
df_30_39 <- datos[datos$grupo_edades == "30-39", ]
df_40_49 <- datos[datos$grupo_edades == "40-49", ]
df_50_59 <- datos[datos$grupo_edades == "50-59", ]
df_60_69 <- datos[datos$grupo_edades == "60-69", ]


###Se crea una nueva columna con valores numéricos para identificar el estado de la paciente, 1 representa el estado vivo y 0 el estado muerto.
datos$Status_Code <- ifelse(datos$Status == "Alive", 1, 0)

###Se crea una nueva columna con valores numéricos para identificar el grado del cancer de la paciente
datos$Grade_Code <- ifelse(datos$Grade == "I", 1,
                          ifelse(datos$Grade == "II", 2,
                                 ifelse(datos$Grade == "III", 3,
                                        ifelse(datos$Grade == "IV", 4, NA))))

###Se modifican los nombres de las columnas
nuevos_nombres <- c("Edad", "Raza", "Grado", "Tamaño_Tumor", "Meses_Sobrevivencia", "Estado", "Grupo_edades", "Cod_Estado", "Cod_Grado")
colnames(datos)<- nuevos_nombres

#Análisis Descriptivo

### Varibale Tamaño del tumor
# Tabla de estadísticos de orden (Mínimo y máximo), Media y Varianza de la variable, por grupo de edad y raza
tabla1 <- datos %>%
    group_by(Grupo_edades, Raza ) %>%
    summarise(Min = min(Tamaño_Tumor),
              Max = max(Tamaño_Tumor),
              Media = mean(Tamaño_Tumor),
              Var = var(Tamaño_Tumor),
              .groups = "drop")
#Observación: además de la raza se considera el factor de la edad de diagnóstico mediante el grupo de edad *falta explicar porque consideramos la edad*
#Histogramas que muestran la distribución del tamaño del tumor por raza
g1 <- ggplot(datos, aes(x = Tamaño_Tumor, fill = Raza)) +
    geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
    labs(
         x = "Tamaño del Tumor (mm)",
         y = "Frecuencia") +
    theme_minimal() +
    scale_fill_manual(values = c("Negra" = "#A2B5CD", "Otra" = "#BCD2EE", "Blanca" = "#6E7B8B"))+
    facet_wrap(~Raza, scales = "free_y") 
print(g1)
#Observación: se presentan en paneles diferentes para poder observar mejor la distribución de cada raza


### Variable Grado
#Tabla que presenta como se distribuyen los diagnosticos por grado en cada raza, con el fin de observar cual grado suele ser el más común en las observaciones
data_grado <- datos %>%
    group_by(Raza, Grado) %>%
    count()
tabla2<- data_grado %>%
    group_by(Raza) %>%
    mutate(Porcentaje = n / sum(n) * 100)
#Observación: la raza "Otra" no tiene observaciones de "Grado IV"
g2 <- ggplot(datos, aes(x = factor(Grado), y = Edad, color = Raza)) +
    geom_point() +
    facet_wrap(~Raza) +
    labs(
        x = "Grado",
        y = "Edad de diagnóstico") +
    theme_minimal() + 
    
    scale_color_manual(values = c("Negra" = "#A2B5CD", "Otra" = "#BCD2EE", "Blanca" = "#6E7B8B"))
print(g2)


### Variable Estado y Edad de diagnóstico 


# Tabla que muestra la cantidad de diagnósticos, el porcentaje de sobrevivientes y de muertes por grupo de edad y raza
tabla3 <- datos %>%
    group_by(Grupo_edades, Raza) %>%
    summarize(Diagnosticos = n(),
              Sobrevivientes = sum(Estado == "Alive") / n() *100,
              Muertes = sum(Estado == "Dead") / n()*100,
              .groups = "drop")

### Variable Meses de Sobrevivencia
#Tabla de estadísticos de orden (Mínimo y máximo), Media y Varianza de la variable, por grupo de edad y raza
tabla4 <- datos %>%
    group_by(Grupo_edades,Raza) %>%
    summarise(Min = min(Meses_Sobrevivencia),
              Max = max(Meses_Sobrevivencia),
              Media = mean(Meses_Sobrevivencia),
              Var = var(Meses_Sobrevivencia),
              .groups = "drop")

### Gráfico de Barras que presenta el Promedio de Meses de Sobrevivencia por Edad y Raza
g3 <- ggplot(datos, aes(x = Grupo_edades, y = Meses_Sobrevivencia, fill = Raza)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    labs(x = "Edad",
         y = "Meses de Sobrevivencia") +
    theme_minimal() +
    scale_fill_manual(values = c("Negra" = "#A2B5CD", "Otra" = "#BCD2EE", "Blanca" = "#6E7B8B"))
print(g3)

### Gráfico de Barras que presenta  el Promedio de Meses de Sobrevivencia por Grado y Raza
g4 <- ggplot(datos, aes(x = Grado, y = Meses_Sobrevivencia, fill = Raza)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    labs(x = "Grado",
         y = "Meses de Sobrevivencia") +
    theme_minimal() +
    scale_fill_manual(values = c("Negra" = "#A2B5CD", "Otra" = "#BCD2EE", "Blanca" = "#6E7B8B"))
print(g4)




# Aplicación de la Metodología

### Se separa la base de datos en tres bases con el fin de obtener la distribución de las variables por grupo de raza
#Raza: Negra 
datos_Negra <- datos %>%
    filter(Raza == "Negra")

#Distribucion de la variable edad de diagnóstico
datos_Edad_N<-datos_Negra$Edad 
dist_age_N<- model_select(datos_Edad_N, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_age_N<- 5.330e+01
sd_age_N<-9.414e+00
nu_age_N<-7.855e+00
xi_age_N<-5.689e-05  

set.seed(123)  # Establecer una semilla para la reproducibilidad
datos_normales <- rnorm(291, mean = mean_age_N, sd = sd_age_N)

# Aplicar una transformación para obtener una distribución SGE
datos_sge <- datos_normales + nu_age_N * (datos_normales^2) * (datos_normales > 0) +
    xi_age_N * datos_normales * (datos_normales <= 0)

hist(datos_sge, breaks = 30, col = "#A2B5CD", xlab = "Valores", ylab = "Frecuencia",
     main = "Histograma de la Distribución SGE")
#Distribucion de la variable Grado
datos_Grado_N<-datos_Negra$Cod_Grado
dist_grade_N<- model_select(datos_Grado_N, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_grade_N<- 2.2942
sd_grade_N<-0.7878
nu_grade_N<-0.9433
xi_grade_N<-0.2766  


#Distribucion de la variable Tamaño del Tumor
datos_Tumor_N<-datos_Negra$Tamaño_Tumor
dist_tumor_N<- model_select(datos_Tumor_N, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_tumor_N<- 30.7871 
sd_tumor_N<-21.8594
nu_tumor_N<-0.9314
xi_tumor_N<-2.0563  

#Distribución de la variable Meses de Sobrevivencia
datos_meses_N<-datos_Negra$Meses_Sobrevivencia
dist_meses_N<- model_select(datos_meses_N, which = "BIC")
#Parámetros MLE: (PoweDist)
alpha_meses_N<- 107.000 
beta_meses_N<-1.698  

#Distribucion de la variable Estado 
datos_Estado_N<-datos_Negra$Cod_Estado
dist_status_N<- model_select(datos_Estado_N, which = "BIC")
### Parámteros MLE: (Distribución Cauchy)
loc_estado_N <-1.000e+00
scale_estado_N <- 1.912e-06


#Raza: Blanca
datos_Blanca <- datos %>%
    filter(Raza == "Blanca")
#Distribucion de la variable edad de diagnóstico
datos_Edad_B<-datos_Blanca$Edad
dist_age_B<- model_select(datos_Edad_B, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_age_B<- 5.469e+01
sd_age_B<-8.964e+00
nu_age_B<-4.840e+00
xi_age_B<-7.352e-04  

#Distribucion de la variable Grado
datos_Grado_B<-datos_Blanca$Cod_Grado
dist_grade_B<- model_select(datos_Grado_B, which = "BIC")
#Parámetros MLE: (Generalized Error)
mean_grade_B<- 2.0000 
sd_grade_B<-0.6987
nu_grade_B<-0.1927 

#Distribucion de la variable Tamaño del Tumor
datos_Tumor_B<-datos_Blanca$Tamaño_Tumor
dist_tumor_B<- model_select(datos_Tumor_B, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_tumor_B<- 30.459 
sd_tumor_B<-20.718
nu_tumor_B<-1.007
xi_tumor_B<-2.275  

#Distribución de la variable Meses de Sobrevivencia
datos_meses_B<-datos_Blanca$Meses_Sobrevivencia
dist_meses_B<- model_select(datos_meses_B, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_meses_B<- 7.230e+01
sd_meses_B<-2.342e+01
nu_meses_B<- 3.081e+00
xi_meses_B<-4.484e-04

#Distribucion de la variable Estado 
datos_Estado_B<-datos_Blanca$Cod_Estado
dist_status_B<- model_select(datos_Estado_B, which = "BIC")
### Parámteros MLE: (Distribución Cauchy)
loc_status_B <-1.000e+00
scale_status_B <- 1.217e-07 


#Raza: Otra
datos_Otra <- datos %>%
    filter(Raza == "Otra")
#Distribucion de la variable edad de diagnóstico
datos_Edad_O<-datos_Otra$Edad
dist_age_O<- model_select(datos_Edad_O, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_age_O<- 51.7013 
sd_age_O<-9.6867
nu_age_O<- 5.7086 
xi_age_O<- 0.5287

#Distribucion de la variable Grado
datos_Grado_O<-datos_Otra$Cod_Grado
dist_grade_O<- model_select(datos_Grado_N, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_grade_O<- 2.2942
sd_grade_O<-0.7878
nu_grade_O<- 0.9433
xi_grade_O<- 0.2766

#Distribucion de la variable Tamaño del Tumor
datos_Tumor_O<-datos_Otra$Tamaño_Tumor
dist_tumor_O<- model_select(datos_Tumor_O, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_tumor_O<- 30.9792
sd_tumor_O<-20.0960
nu_tumor_O<- 0.9925
xi_tumor_O<- 2.3040 

#Distribución de la variable Meses de Sobrevivencia
datos_meses_O<-datos_Otra$Meses_Sobrevivencia
dist_meses_O<- model_select(datos_meses_O, which = "BIC")
#Parámetros MLE: (Skew Generalized Error)
mean_meses_O<- 7.347e+01
sd_meses_O<-2.333e+01
nu_meses_O<-2.695e+00 
xi_meses_O<- 3.086e-04 

#Distribucion de la variable Estado 
datos_Estado_O<-datos_Otra$Cod_Estado
dist_status_O<- model_select(datos_Estado_O, which = "BIC")
### Parámteros MLE: (Distribución t-student)
mean_status_O<-1.000e+00 
sd_status_O<-1.078e-13 
nu_status_O<-3.987e+00 



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

#x<- datos$Tumor.Size
#y <- plogis(log(x / beta_previa) / alpha_previa)

# Create a data frame with x and y values
df <- data.frame(x = x, y = y)

# Plot using ggplot2
ggplot(data = df, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 2) + xlab("Tamaño del Tumor")
  ylab("Cumulative Probability") +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_minimal()
  
  ########
  # Pruebas KS para las diferentes variables Blanca-Negra
  pruebaKS_EdadBN<-ks.test(datos_Edad_N, datos_Edad_B)
  
  pruebaKS_TumorBN<- ks.test(datos_Tumor_N,datos_Tumor_B)
  
  pruebaKS_GradoBN<- ks.test(datos_Grado_N,datos_Grado_B)
  
  pruebaKS_EstadoBN<- ks.test(datos_Estado_N,datos_Estado_B)
  
  pruebaKS_SobrevivenciaBN<- ks.test(datos_meses_N,datos_meses_B)
  ########
  # Pruebas KS para las diferentes variables Otra-Negra
  pruebaKS_EdadON<-ks.test(datos_Edad_N, datos_Edad_O)
  
  pruebaKS_TumorON<- ks.test(datos_Tumor_N,datos_Tumor_O)
  
  pruebaKS_GradoON<- ks.test(datos_Grado_N,datos_Grado_O)
  
  pruebaKS_EstadoON<- ks.test(datos_Estado_N,datos_Estado_O)
  
  pruebaKS_SobrevivenciaON<- ks.test(datos_meses_N,datos_meses_O)
  ########
  # Pruebas KS para las diferentes variables Blanca-Negra
  pruebaKS_EdadBO<-ks.test(datos_Edad_O, datos_Edad_B)
  
  pruebaKS_TumorBO<- ks.test(datos_Tumor_O,datos_Tumor_B)
  
  pruebaKS_GradoBO<- ks.test(datos_Grado_O,datos_Grado_B)
  
  pruebaKS_EstadoBO<- ks.test(datos_Estado_O,datos_Estado_B)
  
  pruebaKS_SobrevivenciaBO<- ks.test(datos_meses_O,datos_meses_B)
  
  ##########################################################################################
  #Gráfico de CDF de Edad por Raza 
  # Compute ECDF for Tumor_N data
  ecdf_N <- ecdf(datos_Edad_N)
  x_N <- sort(datos_Edad_N)
  y_N <- ecdf_N(x_N)
  
  # Compute ECDF for Tumor_B data
  ecdf_B <- ecdf(datos_Edad_B)
  x_B <- sort(datos_Edad_B)
  y_B <- ecdf_B(x_B)
  
  ecdf_O <- ecdf(datos_Edad_O)
  x_O <- sort(datos_Edad_O)
  y_O <- ecdf_O(x_O)
  
  # Create a data frame for ggplot
  df <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), group = rep(c("Edad_N", "Edad_B", "Edad_O"), c(length(x_N), length(x_B), length(x_O))))
  
  # Plot ECDFs using ggplot2
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(size = 0.7) +
    labs(x = "Edad", y = "Cumulative Probability", title = "Comparación de función de distribución de Edad") +
    scale_color_manual(values = c("#A2B5CD", "#6E7B8B","#BCD2EE")) +
    theme_minimal()
  ##########################################################################################
  #Gráfico de CDF del tamano del tumor por Raza 
  # Compute ECDF for Tumor_N data
  ecdf_N <- ecdf(datos_Tumor_N)
  x_N <- sort(datos_Tumor_N)
  y_N <- ecdf_N(x_N)
  
  # Compute ECDF for Tumor_B data
  ecdf_B <- ecdf(datos_Tumor_B)
  x_B <- sort(datos_Tumor_B)
  y_B <- ecdf_B(x_B)
  
  ecdf_O <- ecdf(datos_Tumor_O)
  x_O <- sort(datos_Tumor_O)
  y_O <- ecdf_O(x_O)
  
  # Create a data frame for ggplot
  df <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), group = rep(c("Tamaño_N", "Tamaño_B", "Tamaño_O"), c(length(x_N), length(x_B), length(x_O))))
  
  # Plot ECDFs using ggplot2
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(size = 0.7) +
    labs(x = "Tamaño del Tumor en mm", y = "Cumulative Probability", title = "Comparación de función de distribución de Tamaño del Tumor") +
    scale_color_manual(values = c("#A2B5CD", "#6E7B8B","#BCD2EE")) +
    theme_minimal()
  ##########################################################################################
  #Gráfico de CDF de Grado por Raza 
  # Compute ECDF for Tumor_N data
  ecdf_N <- ecdf(datos_Grado_N)
  x_N <- sort(datos_Grado_N)
  y_N <- ecdf_N(x_N)
  
  # Compute ECDF for Tumor_B data
  ecdf_B <- ecdf(datos_Grado_B)
  x_B <- sort(datos_Grado_B)
  y_B <- ecdf_B(x_B)
  
  ecdf_O <- ecdf(datos_Grado_O)
  x_O <- sort(datos_Grado_O)
  y_O <- ecdf_O(x_O)
  
  # Create a data frame for ggplot
  df <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), group = rep(c("Grado_N", "Grado_B", "Grado_O"), c(length(x_N), length(x_B), length(x_O))))
  
  # Plot ECDFs using ggplot2
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(size = 0.7) +
    labs(x = "Grado", y = "Cumulative Probability", title = "Comparación de función de distribución de Grado") +
    scale_color_manual(values = c("#A2B5CD", "#6E7B8B","#BCD2EE")) +
    theme_minimal()
  ##########################################################################################
  #Gráfico por Estado dde CDF 
   # Compute ECDF for Tumor_N data
  ecdf_N <- ecdf(datos_Estado_N)
  x_N <- sort(datos_Estado_N)
  y_N <- ecdf_N(x_N)
  
  # Compute ECDF for Tumor_B data
  ecdf_B <- ecdf(datos_Estado_B)
  x_B <- sort(datos_Estado_B)
  y_B <- ecdf_B(x_B)
  
  ecdf_O <- ecdf(datos_Estado_O)
  x_O <- sort(datos_Estado_O)
  y_O <- ecdf_O(x_O)
  
  # Create a data frame for ggplot
  df <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), group = rep(c("Estado_N", "Estado_B", "Estado_O"), c(length(x_N), length(x_B), length(x_O))))
  
  # Plot ECDFs using ggplot2
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(size = 0.7) +
    labs(x = "Estado", y = "Cumulative Probability", title = "Comparación de función de distribución de Estado") +
    scale_color_manual(values = c("#A2B5CD", "#6E7B8B","#BCD2EE")) +
    theme_minimal()
  ##########################################################################################
  # Compute ECDF for Tumor_N data
  ecdf_N <- ecdf(datos_meses_N)
  x_N <- sort(datos_meses_N)
  y_N <- ecdf_N(x_N)
  
  # Compute ECDF for Tumor_B data
  ecdf_B <- ecdf(datos_meses_B)
  x_B <- sort(datos_meses_B)
  y_B <- ecdf_B(x_B)
  
  ecdf_O <- ecdf(datos_meses_O)
  x_O <- sort(datos_meses_O)
  y_O <- ecdf_O(x_O)
  
  # Create a data frame for ggplot
  df <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), group = rep(c("Sobrevivencia_N", "Sobrevivencia_B", "Sobrevivencia_O"), c(length(x_N), length(x_B), length(x_O))))
  
  # Plot ECDFs using ggplot2
  ggplot(df, aes(x = x, y = y, color = group)) +
    geom_line(size = 0,7) +
    labs(x = "Sobrevivencia (meses)", y = "Cumulative Probability", title = "Comparación de función de distribución de Sobrevivencia") +
    scale_color_manual(values = c("#A2B5CD", "#6E7B8B","#BCD2EE")) +
    theme_minimal()