#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
## Naïve Bayes classifier                          ##
#####################################################
library (e1071)

# Leemos los datos ya separados en train y test
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)
data.train <- read.csv(file = "data.train.csv", header = TRUE)
data.test <- read.csv(file = "data.test.csv", header = TRUE)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)
data.train <- as.data.frame(data.train)
data.test <- as.data.frame(data.test)

# Construimos el modelo con data.train
model <- naiveBayes(diagnosis ~ ., data = data.train)

# Error train
pred <- predict(model, data.train)

tab <- table(pred, data.train$diagnosis) 
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# Error de test
pred <- predict(model, newdata=data.test)

tab <- table(pred, data.test$diagnosis) 
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))