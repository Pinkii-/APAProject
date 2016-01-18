setwd("~/Documents/APAProject")
data <- read.csv("data.csv",header=TRUE)

summary(data)

# Eliminamos la columna de los ids
data$id <- NULL

# Separamos aleatoriamente en dos conjuntos. Conjunto de test y conjunto de entrenamiento.
set.seed(42)
learn <- sample(1:nrow(data), round(2*nrow(data)/3))


# Guardamos los datos limpios
write.table(data, file = "data.clean.csv",row.names=FALSE, col.names=TRUE, sep=",")
# Elegimos dos tercios aleatorios de la muestra para train
write.table(data[learn,], file = "data.train.csv",row.names=FALSE, col.names=TRUE, sep=",")
# Elegimos el tercio sobrante para testear los modelos
write.table(data[-learn,], file = "data.test.csv",row.names=FALSE, col.names=TRUE, sep=",")

