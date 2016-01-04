setwd("~/Documents/APAProject")
data <- read.csv("data.csv",header=FALSE)

# Eliminamos los valores perdidos
data <- data[data$V7 != "?",]
# Convertimos la columna a integers
data$V7 <- as.numeric(data$V7)
# Eliminamos la columna de los ids
data$V1 <- NULL

learn <- sample(1:nrow(data), round(2*nrow(data)/3))

# Elegimos dos tercios aleatorios de la muestra para train
write.table(data[learn,], file = "data.train.csv",row.names=FALSE, col.names=FALSE, sep=",")
# Elegimos el tercio sobrante para testear los modelos
write.table(data[-learn,], file = "data.test.csv",row.names=FALSE, col.names=FALSE, sep=",")

