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