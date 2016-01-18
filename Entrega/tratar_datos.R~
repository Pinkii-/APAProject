setwd("~/Documents/APAProject")
data <- read.csv("data.csv",header=TRUE)

# Vamos a hecharle un vistazo por si las moscas
summary(data)

# Eliminamos los valores perdidos
# data <- data[data$nucleos_bare != "?",]

# Convertimos la columna a integers
# data$nucleos_bare <- as.numeric(data$nucleos_bare)

# Eliminamos la columna de los ids
data$id <- NULL

data.three <- data.frame(data$diagnosis,data$largest.area,data$largest.smoothness,data$mean.texture)

# Separamos aleatoriamente en dos conjuntos. Conjunto de test y conjunto de entrenamiento.
set.seed(42)
learn <- sample(1:nrow(data), round(2*nrow(data)/3))


# Guardamos los datos limpios
write.table(data, file = "data.clean.csv",row.names=FALSE, col.names=TRUE, sep=",")
# Elegimos dos tercios aleatorios de la muestra para train
write.table(data[learn,], file = "data.train.csv",row.names=FALSE, col.names=TRUE, sep=",")
# Elegimos el tercio sobrante para testear los modelos
write.table(data[-learn,], file = "data.test.csv",row.names=FALSE, col.names=TRUE, sep=",")

# Elegimos dos tercios aleatorios de la muestra para train
write.table(data.three[learn,], file = "data.train3.csv",row.names=FALSE, col.names=TRUE, sep=",")
# Elegimos el tercio sobrante para testear los modelos
write.table(data.three[-learn,], file = "data.test3.csv",row.names=FALSE, col.names=TRUE, sep=",")
