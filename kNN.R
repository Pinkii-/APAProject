#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
## LDA y QDA                                       ##
#####################################################
library (class)

# Leemos los datos ya separados en train y test
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)
data.train <- read.csv(file = "data.train.csv", header = TRUE)
data.test <- read.csv(file = "data.test.csv", header = TRUE)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)
data.train <- as.data.frame(data.train)
data.test <- as.data.frame(data.test)

learn.inputs <- data.train[,2:31]
learn.classes <- data.train[,1]

test.inputs <- data.test[,2:31]
test.classes <- data.test[,1]

## Vamos a buscar un número de vecinos óptimo

## Let's loop over k
set.seed (23)

neighbours <- c(1:20)
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (learn.inputs, learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

min(errors)
errors

## Hemos ejecutado varias veces este codigo y en todas ellas el número óptimo cambia
## de todos modos en general se establece entre el 3 y el 6
## Hemos decidido fijarlo en 4

myknn <- knn (learn.inputs, test.inputs, learn.classes, k = 4, prob=TRUE) 

tab <- table(myknn, test.classes) 
tab
(error <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))