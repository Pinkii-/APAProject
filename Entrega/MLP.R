#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
## MLP (nnet)                                      ##
#####################################################
library(MASS)
library(nnet)

# Leemos los datos ya separados en train y test
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)
data.train <- read.csv(file = "data.train.csv", header = TRUE)
data.test <- read.csv(file = "data.test.csv", header = TRUE)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)
data.train <- as.data.frame(data.train)
data.test <- as.data.frame(data.test)

# Escalando los datos
for (i in 2:length(data.clean)) {
  data.train[,i] <- scale(data.train[,i])
  data.test[,i] <- scale(data.test[,i])
}

# Generando el modelo con 2 neuronas por probar algo
model.nnet <- nnet(diagnosis ~., data = data.train, size=2, maxit=200, decay=0)

# Vamos a ver el error de train de predecir la clase B o M
p1 <- as.factor(predict (model.nnet, type = "class"))
t1 <- table(p1,data.train$diagnosis)
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(data.train))
error_rate.learn

## Sorprendentemente bajo, ¿es posible que haya overfitting?

# Vamos a ver el error de test de predecir la clase B o M
p2 <- as.factor(predict (model.nnet, newdata=data.test, type="class"))
(t2 <- table(p2,data.test$diagnosis))
error_rate.test <- 100*(1-sum(diag(t2))/nrow(data.test))
error_rate.test

## Mas alto que el de train (bastante) overfitting?

## Vamos a ver la diferencia entre predecir la clase (discretizar la salida) y la salida raw
model.nnet <- nnet(diagnosis ~., data = data.train, size=20, maxit=200)

# Error de train sin discretizar la salida (en raw)
p1 <- as.factor(predict (model.nnet, type="raw"))
(t1 <- table(p1,data.train$diagnosis))
error_rate.learn <- 100*(1-sum(diag(t1))/nrow(data.train))
error_rate.learn

# Error de test sin discretizar la salida (en raw)
p2 <- as.factor(predict (model.nnet, newdata=data.test, type="raw"))
(t2 <- table(p2,data.test$diagnosis))
error_rate.test <- 100*(1-sum(diag(t2))/nrow(data.test))
error_rate.test

## Estos dos errores son parecidos entre si 
## Curiosamente tienen la misma diferencia que el error de predecir la clase
## Pese a que la diferencia etre raw y class es muy alta no veo problemas dado que en realidad
## los datos se discretizan en casos muy claros. No nos encontramos un 0.49999 convertido en un 0 por ejemplo
## Ya que precisamente lo que queremos predecir es la clase y no a la inversa.
## p1                       B   M
## 0                    231   0
## 3.45249441714434e-07   1   0
## 2.14419717950265e-06   1   0
## 2.15185572806224e-06   1   0
## 4.55085150221023e-06   1   0
## 5.31148710105192e-06   1   0
## 2.17240964816293e-05   1   0
## 0.9999662340364        0   1
## 0.999975258388252      0   1
## 0.999997997075748      0   1
## 0.999999170085833      0   1
## 1                      0 138
## La discretización es mínima y necesaria

## Estudiar el número de neuronas optimo para la capa oculta y la caída
library(caret)

# Estudio del número de neuronas (size)
(sizes <- 2*seq(1,10,by=1))

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10)

model.10x10CV.size <- train (diagnosis ~., data = data.train, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)

## We can inspect the full results
model.10x10CV.size$results

## and the best model found
model.10x10CV.size$bestTune

# Después de varias ejecuciones vemos que la diferencia entre unas y otras es mínima, a veces 2 es suficiente
# y otras veces el mejor resultado es 20, Todos los tamaños tienen una precisión de ~0.95
# El caso mas habitual es 10 Por lo tanto en el resto de tests vamos a usar 10

# Error de train
p1 <- as.factor(predict (model.10x10CV.size, newdata=data.train, type="raw"))
(t1 <- table(p1,data.train$diagnosis))
error_rate.test <- 100*(1-sum(diag(t1))/nrow(data.train))
error_rate.test

# Error de test
p2 <- as.factor(predict (model.10x10CV.size, newdata=data.test, type="raw"))
(t2 <- table(p2,data.test$diagnosis))
error_rate.test <- 100*(1-sum(diag(t2))/nrow(data.test))
error_rate.test

# En cualquier caso el error es menor que sin el entrenamiento 

## Estudio de la caida (decay)
(decays <- 10^seq(-3,0,by=0.1))

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10)

# Dado que la precisión es altamente parecida con dos que son 20 neuronas vamos a hacer el testo con 2 neuronas
model.10x10CV.decay <- train (diagnosis ~., data = data.train, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=10,.decay=decays), trControl=trc)

## We can inspect the full results
model.10x10CV.decay$results

## and the best model found
model.10x10CV.decay$bestTune

## De igual modo que con el número de neuronas la precisión entre las distintas caídas es prácticamente la misma,
## Pero la mejor solución habitualmente es la de caida=1 En el resto de tests vamos a usar decay=1

# Error de train
p1 <- as.factor(predict (model.10x10CV.decay, newdata=datdata.train, type="raw"))
t1 <- table(pred=p1,truth=data.train$diagnosis)
error_rate.test <- 100*(1-sum(diag(t1))/nrow(data.train))
error_rate.test

# Error de test
p2 <- as.factor(predict (model.10x10CV.decay, newdata=data.test, type="raw"))
t2 <- table(pred=p2,truth=data.test$diagnosis)
error_rate.test <- 100*(1-sum(diag(t2))/nrow(data.test))
error_rate.test

# Conforme hemos avanzado en el training primero capas y luego caida hemos reducido el error.

## Vamos a ver la diferencia entre predecir la clase y el raw
model.nnet <- nnet(diagnosis ~., data = data.train, size=10, maxit=500, decay=1)

# Error de train
p1 <- as.factor(predict (model.nnet, newdata=data.train, type="class"))
(t1 <- table(pred=p1,truth=data.train$diagnosis))
error_rate.train <- 100*(1-sum(diag(t1))/nrow(data.train))
error_rate.train

# Error de test
p2 <- as.factor(predict (model.nnet, newdata=data.test, type="class"))
(t2 <- table(pred=p2,truth=data.test$diagnosis))
error_rate.test <- 100*(1-sum(diag(t2))/nrow(data.test))
error_rate.test
