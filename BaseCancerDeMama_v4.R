# Paquetes
library(dplyr)
library(tidyverse)
library(ggplot2)

# Lectura de la base de datos
datos <- read.csv("/Users//mariabolanosgutierrez/Downloads/dataCancer.csv")

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

count_black <- sum(grepl("Negra", datos$Race, ignore.case = TRUE))

count_white <- sum(grepl("Blanca", datos$Race, ignore.case = TRUE))

count_other <- sum(grepl("Otra", datos$Race, ignore.case = TRUE))


# Rango de las edades de diagnóstico en estudio
youngest_age <- min(datos$Age)
oldest_age <- max(datos$Age)
mean_age<- mean(datos$Age)

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
    scale_fill_manual(values = c("Negra" = "#53868B", "Otra" = "#A2CD5A", "Blanca" = "#CD5B45"))+
    facet_wrap(~Raza, scales = "free_y") 
print(g1)
ggsave(filename = "g1_tumor.pdf", plot = g1, width = 6, height = 4)
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
    labs(
        x = "Grado",
        y = "Edad de diagnóstico") +
    theme_minimal() + 
    scale_color_manual(values = c("Negra" = "#53868B", "Otra" = "#A2CD5A", "Blanca" = "#CD5B45")) +
    facet_wrap(~Raza)
print(g2)
ggsave(filename = "g2_Grado.pdf", plot = g2, width = 6, height = 4)

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
    scale_fill_manual(values = c("Negra" = "#53868B", "Otra" = "#A2CD5A", "Blanca" = "#CD5B45"))
print(g3)
ggsave(filename = "g3_MesesSobrevivencia1.pdf", plot = g3, width = 6, height = 4)

### Gráfico de Barras que presenta  el Promedio de Meses de Sobrevivencia por Grado y Raza
g4 <- ggplot(datos, aes(x = Grado, y = Meses_Sobrevivencia, fill = Raza)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +
    labs(x = "Grado",
         y = "Meses de Sobrevivencia") +
    theme_minimal() +
    scale_fill_manual(values = c("Negra" = "#53868B", "Otra" = "#A2CD5A", "Blanca" = "#CD5B45"))
print(g4)
ggsave(filename = "g4_MesesSobrevivencia2.pdf", plot = g4, width = 6, height = 4)




# Aplicación de la Metodología

### Se separa la base de datos en tres bases 
#Raza: Negra 
datos_Negra <- datos %>%
    filter(Raza == "Negra")
datos_Edad_N<-datos_Negra$Edad 
datos_Grado_N<-datos_Negra$Cod_Grado
datos_Tumor_N<-datos_Negra$Tamaño_Tumor
datos_meses_N<-datos_Negra$Meses_Sobrevivencia
datos_Estado_N<-datos_Negra$Cod_Estado
#Raza: Blanca
datos_Blanca <- datos %>%
    filter(Raza == "Blanca")
datos_Edad_B<-datos_Blanca$Edad
datos_Grado_B<-datos_Blanca$Cod_Grado
datos_Tumor_B<-datos_Blanca$Tamaño_Tumor
datos_meses_B<-datos_Blanca$Meses_Sobrevivencia
datos_Estado_B<-datos_Blanca$Cod_Estado
#Raza: Otra
datos_Otra <- datos %>%
    filter(Raza == "Otra")
datos_Edad_O<-datos_Otra$Edad
datos_Grado_O<-datos_Otra$Cod_Grado
datos_Tumor_O<-datos_Otra$Tamaño_Tumor
datos_meses_O<-datos_Otra$Meses_Sobrevivencia
datos_Estado_O<-datos_Otra$Cod_Estado

# Pruebas KS para las diferentes variables de las razas: Blanca y Negra
  pruebaKS_EdadBN<-ks.test(datos_Edad_N, datos_Edad_B)
  
  pruebaKS_TumorBN<- ks.test(datos_Tumor_N,datos_Tumor_B)
  
  pruebaKS_GradoBN<- ks.test(datos_Grado_N,datos_Grado_B)
  
  pruebaKS_EstadoBN<- ks.test(datos_Estado_N,datos_Estado_B)
  
  pruebaKS_SobrevivenciaBN<- ks.test(datos_meses_N,datos_meses_B)
 
# Pruebas KS para las diferentes variables  de las razas: Otra y Negra
  pruebaKS_EdadON<-ks.test(datos_Edad_N, datos_Edad_O)
  
  pruebaKS_TumorON<- ks.test(datos_Tumor_N,datos_Tumor_O)
  
  pruebaKS_GradoON<- ks.test(datos_Grado_N,datos_Grado_O)
  
  pruebaKS_EstadoON<- ks.test(datos_Estado_N,datos_Estado_O)
  
  pruebaKS_SobrevivenciaON<- ks.test(datos_meses_N,datos_meses_O)

# Pruebas KS para las diferentes variables  de las razas: Otra y Blanca
  pruebaKS_EdadBO<-ks.test(datos_Edad_O, datos_Edad_B)
  
  pruebaKS_TumorBO<- ks.test(datos_Tumor_O,datos_Tumor_B)
  
  pruebaKS_GradoBO<- ks.test(datos_Grado_O,datos_Grado_B)
  
  pruebaKS_EstadoBO<- ks.test(datos_Estado_O,datos_Estado_B)
  
  pruebaKS_SobrevivenciaBO<- ks.test(datos_meses_O,datos_meses_B)
  
#Funciones de distribución empírica acumulada
###Variable Edad de diagnóstico
#### Datos Raza Negra
  ecdf_N <- ecdf(datos_Edad_N)
  x_N <- sort(datos_Edad_N)
  y_N <- ecdf_N(x_N)
#####Datos Raza Blanca
  ecdf_B <- ecdf(datos_Edad_B)
  x_B <- sort(datos_Edad_B)
  y_B <- ecdf_B(x_B)
#####Datos Raza Otra
  ecdf_O <- ecdf(datos_Edad_O)
  x_O <- sort(datos_Edad_O)
  y_O <- ecdf_O(x_O)

 #Data frame con todos los datos de distribución de la edad de diagnóstico 
  df1 <- data.frame(x = c(x_N, x_B, x_O), y = c(y_N, y_B,y_O), 
                    Raza= rep(c("Negra", "Blanca", "Otra"), 
                              c(length(x_N), length(x_B), length(x_O))))

 ##### Gráfico de las funciones
 g5<- ggplot(df1, aes(x = x, y = y, color = Raza)) +
    geom_line(size = 0.7) +
    labs(x = "Edad", y = "Probabilidad") +
     scale_color_manual(values = c("#CD5B45", "#53868B","#A2CD5A")) +
    theme_minimal()
 print(g5)
 ggsave(filename = "g5_dist_Edad.pdf", plot = g5, width = 6, height = 4)
 
 ###Variable Tamaño del tumor
 #### Datos Raza Negra
  ecdf_N2 <- ecdf(datos_Tumor_N)
  x_N2 <- sort(datos_Tumor_N)
  y_N2 <- ecdf_N(x_N2)
  #### Datos Raza Blanca
  ecdf_B2 <- ecdf(datos_Tumor_B)
  x_B2 <- sort(datos_Tumor_B)
  y_B2<- ecdf_B(x_B2)
  #### Datos Raza Otra
  ecdf_O2 <- ecdf(datos_Tumor_O)
  x_O2 <- sort(datos_Tumor_O)
  y_O2 <- ecdf_O(x_O2)
  
  ##Data frame con todos los datos de distribución del tamaño de tumor 
  df2 <- data.frame(x = c(x_N2, x_B2, x_O2), y = c(y_N2, y_B,y_O2),
                    Raza = rep(c("Negra", "Blanca", "Otra"), c(length(x_N2), 
                                                               length(x_B2), 
                                                               length(x_O2))))
  
  #Gráfico de las funciones
  g6<- ggplot(df, aes(x = x, y = y, color = Raza)) +
    geom_line(size = 0.7) +
    labs(x = "Tamaño del Tumor(mm)", y = "Probabilidad") +
      scale_color_manual(values = c("#CD5B45", "#53868B","#A2CD5A")) +
      theme_minimal()
      print(g6)
  ggsave(filename = "g6_dist_Tumor.pdf", plot = g6, width = 6, height = 4)
 
  ###Variable Grado del Tumor
  #### Datos Raza Negra
  ecdf_N3 <- ecdf(datos_Grado_N)
  x_N3 <- sort(unique(datos_Grado_N))
  y_N3 <- ecdf_N3(x_N3)
  #### Datos Raza Blanca
  ecdf_B3 <- ecdf(datos_Grado_B)
  x_B3 <- sort(unique(datos_Grado_B))
  y_B3 <- ecdf_B3(x_B3)
  #### Datos Raza Otra
  ecdf_O3 <- ecdf(datos_Grado_O)
  x_O3 <- sort(unique(datos_Grado_O))
  y_O3 <- ecdf_O3(x_O3)
  
  
  ####Data frame con todos los datos de distribución del grado del tumor 
  df3 <- data.frame(x = c(x_N3, x_B3, x_O3), y = c(y_N3, y_B3, y_O3), 
                    Raza = rep(c("Negra", "Blanca", "Otra"), c(length(x_N3), 
                                                               length(x_B3), 
                                                               length(x_O3))))
  
  #Gráfico de las funciones
  g7 <- ggplot(df3, aes(x = x, y = y, color = Raza)) +
      geom_line(size = 0.7) +
      labs(x = "Grado", y = "Probabilidad") +
      scale_color_manual(values = c("#CD5B45", "#53868B", "#A2CD5A")) +
      theme_minimal()
  
 print(g7)
 ggsave(filename = "g7_dist_Grado.pdf", plot = g7, width = 6, height = 4)
  
 
 ###Variable Estado
 #### Datos Raza Negra
 ecdf_N4 <- ecdf(datos_Estado_N)
 x_N4 <- sort(unique(datos_Estado_N)) 
 y_N4 <- ecdf_N4(x_N4)
 
 ecdf_B4 <- ecdf(datos_Estado_B)
 x_B4 <- sort(unique(datos_Estado_B))
 y_B4 <- ecdf_B4(x_B4)
 
 ecdf_O4 <- ecdf(datos_Estado_O)
 x_O4 <- sort(unique(datos_Estado_O))
 y_O4 <- ecdf_O4(x_O4)
  
 #Data frame con todos los datos de distribución del estado segmentado por raza
 df4 <- data.frame(x = c(x_N4, x_B4, x_O4), y = c(y_N4, y_B4, y_O4), 
                   Raza = rep(c("Negra", "Blanca", "Otra"), c(length(x_N4),
                                                              length(x_B4), 
                                                              length(x_O4))))
 #Gráfico de las funciones
 g8 <- ggplot(df4, aes(x = x, y = y, color = Raza)) +
     geom_line(size = 0.7) +
     labs(x = "Estado", y = "Probabilidad") +
     scale_color_manual(values = c("#CD5B45", "#53868B", "#A2CD5A")) +
     theme_minimal()
 print(g8)
 ggsave(filename = "g8_dist_Estado.pdf", plot = g8, width = 6, height = 4)
  
 
 ###Variable Meses de Sobrevivencia
 #### Datos Raza Negra
 ecdf_N5 <- ecdf(datos_meses_N)
 x_N5 <- sort(datos_meses_N)
 y_N5 <- ecdf_N5(x_N5)
 #### Datos Raza Blanca
 ecdf_B5 <- ecdf(datos_meses_B)
 x_B5 <- sort(datos_meses_B)
 y_B5 <- ecdf_B5(x_B5)
 #### Datos Raza Otra
 ecdf_O5 <- ecdf(datos_meses_O)
 x_O5 <- sort(datos_meses_O)
 y_O5 <- ecdf_O5(x_O5)
  
  #Data frame con todos los datos de distribución de los meses de sobrevivencia 
 df5 <- data.frame(x = c(x_N5, x_B5, x_O5), y = c(y_N5, y_B5, y_O5),
                   Raza = rep(c("Negra", "Blanca", "Otra"), 
                              c(length(x_N5), length(x_B5), length(x_O5))))
 
  #Gráfico de las funciones
 g9 <- ggplot(df5, aes(x = x, y = y, color = Raza)) +
     geom_line(size = 0.7) + 
     labs(x = "Meses de Sobrevivencia", y = "Probabilidad") +
     scale_color_manual(values = c("#CD5B45", "#53868B", "#A2CD5A")) +
     theme_minimal()
  print(g9)
  ggsave(filename = "g9_dist_MesesSob.pdf", plot = g9, width = 6, height = 4)
 
  
  #Pruebas Chi- Cuadro
  ## Factor Edad de diagnostico 
  df_Raza_Edad<- table(datos$Raza, datos$Edad)
  resultado_chi_Edad<-chisq.test(df_Raza_Edad)
  print(resultado_chi_Edad)
  ## Factor Tamaño del Tumor
  df_Raza_Tumor<- table(datos$Raza, datos$Tamaño_Tumor)
  resultado_chi_Tumor<-chisq.test(df_Raza_Tumor)
  print(resultado_chi_Tumor)
  ## Factor Grado del Tumor
  df_Raza_Grado<- table(datos$Raza, datos$Grado)
  resultado_chi_Grado<-chisq.test(df_Raza_Grado)
  print(resultado_chi_Grado)
  ## Factor Meses de Sobrevivencia
  df_Raza_Meses<- table(datos$Raza, datos$Meses_Sobrevivencia)
  resultado_chi_Meses<-chisq.test(df_Raza_Meses)
  print(resultado_chi_Meses)
  ## Factor Estado
  df_Raza_Estado<- table(datos$Raza, datos$Estado)
  resultado_chi_Estado<-chisq.test(df_Raza_Estado)
  print(resultado_chi_Estado)
 
  
