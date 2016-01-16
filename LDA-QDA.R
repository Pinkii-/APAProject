#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
## LDA y QDA                                       ##
#####################################################
library(MASS)

# Leemos los datos ya separados en train y test
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)
data.train <- read.csv(file = "data.train.csv", header = TRUE)
data.test <- read.csv(file = "data.test.csv", header = TRUE)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)
data.train <- as.data.frame(data.train)
data.test <- as.data.frame(data.test)

## LDA

# LDA sin LOOCV (leave-one-out cross-validation)
data.lda <- lda(diagnosis ~ ., prior = c(1,1)/2, data = data.train)
predict(data.lda, data.train)$class

# Error?
tab <- table(data.train$diagnosis, predict(data.lda, data.train)$class)
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# LDA con LOOCV (leave-one-out cross-validation)
data.lda.cv <- lda(diagnosis ~ ., prior = c(1,1)/2, data = data.train, CV=TRUE)
data.lda.cv$class

# Vamos a ver el error
tab <- table(data.train$diagnosis, data.lda.cv$class)
tab
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# LDA sin LOOCV tiene un error del ~3%

## QDA

# QDA sin LOOCV
data.qda <- qda(diagnosis ~ ., prior = c(1,1)/2, data = data.train)
predict(data.qda, data.train)$class

tab <- table(data.train$diagnosis, predict(data.qda, data.train)$class)
tab
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# QDA con LOOCV
data.qda.cv <- qda(diagnosis ~ ., prior = c(1,1)/2, data = data.train, CV=TRUE)
summary(data.qda.cv$class)

tab <- table(data.train$diagnosis, data.qda.cv$class)
tab
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# EL error mas pequeño lo tiene el modelo QDA sin LOOCV
# Vomos a ver que sucede con los datos de test

data.qda.final.test <- lda(diagnosis ~ ., prior = c(1,1)/2, data = data.train)
predict(data.qda.final.test, data.test)$class

# Error?
tab <- table(data.test$diagnosis, predict(data.qda.final.test, data.test)$class)
(error.final.test <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))