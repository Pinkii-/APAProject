#set.seed(43) # Definimos una seed para poder reproducir la ejecucion

library(e1071)
require(ggplot2)
require(reshape)

setwd("~/Documents/APAProject")
dataset <- read.csv("data.train.csv",header=TRUE)

dataset.scaled <- cbind(dataset[,1],  data.frame(scale(dataset[,2:length(dataset)])))


summary(dataset)
summary(dataset.scaled)

dataset <- dataset.scaled

N = lengths(dataset)[1]

k <- N
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

train.svm.kCV <- function (which.kernel, myC, kCV=k, degr)
{
  for (i in 1:kCV) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,2:length(dataset)]
    t_train <- train[,1]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="linear", scale = TRUE)},
           poly={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=degr, coef0=1, scale = TRUE)},
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="radial", scale = TRUE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,2:length(dataset)]
    pred <- predict(model,x_valid)
    t_true <- valid[,1]
    
    # compute validation error for part 'i'
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  100*sum(valid.error)/length(valid.error)
}

train.svm.kCV.gamma <- function (which.kernel, myC, kCV=k, degr, myGamma)
{
  for (i in 1:kCV) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,2:length(dataset)]
    t_train <- train[,1]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="linear", scale = TRUE)},
           poly={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=degr, coef0=1, scale = TRUE, gamma = myGamma)},
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="radial", scale = TRUE, gamma = myGamma)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,2:length(dataset)]
    pred <- predict(model,x_valid)
    t_true <- valid[,1]
    
    # compute validation error for part 'i'
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  100*sum(valid.error)/length(valid.error)
}






#LOOCV
#k <- N 
#folds <- sample(rep(1:k, length=N), N, replace=FALSE)

C0001 <- 0.001

VA.error.c0001.linear <- train.svm.kCV ("linear", myC=C0001,kCV = k)
VA.error.c0001.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C0001)
VA.error.c0001.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C0001)
VA.error.c0001.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C0001)
VA.error.c0001.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C0001)
VA.error.c0001.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C0001)
VA.error.c0001.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C0001)
VA.error.c0001.RBF <- train.svm.kCV ("RBF", myC=C0001)

C001 <- 0.01

VA.error.c001.linear <- train.svm.kCV ("linear", myC=C001,kCV = k)
VA.error.c001.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C001)
VA.error.c001.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C001)
VA.error.c001.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C001)
VA.error.c001.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C001)
VA.error.c001.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C001)
VA.error.c001.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C001)
VA.error.c001.RBF <- train.svm.kCV ("RBF", myC=C001)

C01 <- 0.1

VA.error.c01.linear <- train.svm.kCV ("linear", myC=C01,kCV = k)
VA.error.c01.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C01)
VA.error.c01.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C01)
VA.error.c01.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C01)
VA.error.c01.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C01)
VA.error.c01.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C01)
VA.error.c01.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C01)
VA.error.c01.RBF <- train.svm.kCV ("RBF", myC=C01)

C1 <- 1

VA.error.c1.linear <- train.svm.kCV ("linear", myC=C1,kCV = k)
VA.error.c1.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C1)
VA.error.c1.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C1)
VA.error.c1.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C1)
VA.error.c1.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C1)
VA.error.c1.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C1)
VA.error.c1.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C1)
VA.error.c1.RBF <- train.svm.kCV ("RBF", myC=C1)

C10 <- 10

VA.error.c10.linear <- train.svm.kCV ("linear", myC=C10,kCV = k)
VA.error.c10.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C10)
VA.error.c10.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C10)
VA.error.c10.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C10)
VA.error.c10.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C10)
VA.error.c10.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C10)
VA.error.c10.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C10)
VA.error.c10.RBF <- train.svm.kCV ("RBF", myC=C10)

C100 <- 100

VA.error.c100.linear <- train.svm.kCV ("linear", myC=C100,kCV = k)
VA.error.c100.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C100)
VA.error.c100.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C100)
VA.error.c100.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C100)
VA.error.c100.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C100)
VA.error.c100.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C100)
VA.error.c100.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C100)
VA.error.c100.RBF <- train.svm.kCV ("RBF", myC=C100)

C1000 <- 1000

VA.error.c1000.linear <- train.svm.kCV ("linear", myC=C1000,kCV = k)
VA.error.c1000.poly.2 <- train.svm.kCV ("poly",degr = 2, myC=C1000)
VA.error.c1000.poly.3 <- train.svm.kCV ("poly",degr = 3, myC=C1000)
VA.error.c1000.poly.4 <- train.svm.kCV ("poly",degr = 4, myC=C1000)
VA.error.c1000.poly.5 <- train.svm.kCV ("poly",degr = 5, myC=C1000)
VA.error.c1000.poly.6 <- train.svm.kCV ("poly",degr = 6, myC=C1000)
VA.error.c1000.poly.7 <- train.svm.kCV ("poly",degr = 7, myC=C1000)
VA.error.c1000.RBF <- train.svm.kCV ("RBF", myC=C1000)

C.0.001 <- c(VA.error.c0001.linear,VA.error.c0001.poly.2,VA.error.c0001.poly.3,VA.error.c0001.poly.4,VA.error.c0001.poly.5,VA.error.c0001.poly.6,VA.error.c0001.poly.7,VA.error.c0001.RBF)

C.0.01 <- c(VA.error.c001.linear,VA.error.c001.poly.2,VA.error.c001.poly.3,VA.error.c001.poly.4,VA.error.c001.poly.5,VA.error.c001.poly.6,VA.error.c001.poly.7,VA.error.c001.RBF)

C.0.1 <- c(VA.error.c01.linear,VA.error.c01.poly.2,VA.error.c01.poly.3,VA.error.c01.poly.4,VA.error.c01.poly.5,VA.error.c01.poly.6,VA.error.c01.poly.7,VA.error.c01.RBF)

C.1 <- c(VA.error.c1.linear,VA.error.c1.poly.2,VA.error.c1.poly.3,VA.error.c1.poly.4,VA.error.c1.poly.5,VA.error.c1.poly.6,VA.error.c1.poly.7,VA.error.c1.RBF)

C.10 <- c(VA.error.c10.linear,VA.error.c10.poly.2,VA.error.c10.poly.3,VA.error.c10.poly.4,VA.error.c10.poly.5,VA.error.c10.poly.6,VA.error.c10.poly.7,VA.error.c10.RBF)

C.100 <- c(VA.error.c100.linear,VA.error.c100.poly.2,VA.error.c100.poly.3,VA.error.c100.poly.4,VA.error.c100.poly.5,VA.error.c100.poly.6,VA.error.c100.poly.7,VA.error.c100.RBF)

C.1000 <- c(VA.error.c1000.linear,VA.error.c1000.poly.2,VA.error.c1000.poly.3,VA.error.c1000.poly.4,VA.error.c1000.poly.5,VA.error.c1000.poly.6,VA.error.c1000.poly.7,VA.error.c1000.RBF)

names <- c("Linear", "Poly 2", "Poly 3", "Poly 4", "Poly 5", "Poly 6", "Poly 7", "RDF")

results <- data.frame(names,C.0.001,C.0.01,C.0.1,C.1,C.10,C.100,C.1000)

df <- melt(results ,  id.vars = 'names', variable_name = 'C')

ggplot(df,aes(names,value,group = C, colour = C)) + geom_line()

# Quitando C = 0.001 y C = 0.01, ya que su 10-fold CV es demasiado elevado

results <- data.frame(names,C.0.1,C.1,C.10,C.100,C.1000)

df <- melt(results ,  id.vars = 'names', variable_name = 'C')

ggplot(df,aes(names,value,group = C, colour = C)) + geom_line()

# Vemos que con C = 1 es cuando mas veces es minimo, así que usaremos este valor para optimizar la gamma

g00625 <- 0.0625

VA.error.g00625.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g00625.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g00625)
VA.error.g00625.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g00625)
VA.error.g00625.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g00625)
VA.error.g00625.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g00625)
VA.error.g00625.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g00625)
VA.error.g00625.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g00625)
VA.error.g00625.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g00625)

g0125 <- 0.125

VA.error.g0125.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g0125.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g0125)
VA.error.g0125.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g0125)
VA.error.g0125.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g0125)
VA.error.g0125.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g0125)
VA.error.g0125.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g0125)
VA.error.g0125.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g0125)
VA.error.g0125.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g0125)

g025 <- 0.25

VA.error.g025.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g025.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g025)
VA.error.g025.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g025)
VA.error.g025.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g025)
VA.error.g025.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g025)
VA.error.g025.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g025)
VA.error.g025.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g025)
VA.error.g025.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g025)

g05 <- 0.5

VA.error.g05.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g05.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g05)
VA.error.g05.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g05)
VA.error.g05.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g05)
VA.error.g05.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g05)
VA.error.g05.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g05)
VA.error.g05.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g05)
VA.error.g05.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g05)

g1 <- 1

VA.error.g1.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g1.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g1)
VA.error.g1.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g1)
VA.error.g1.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g1)
VA.error.g1.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g1)
VA.error.g1.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g1)
VA.error.g1.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g1)
VA.error.g1.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g1)

g2 <- 2

VA.error.g2.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g2.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g2)
VA.error.g2.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g2)
VA.error.g2.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g2)
VA.error.g2.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g2)
VA.error.g2.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g2)
VA.error.g2.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g2)
VA.error.g2.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g2)

g4 <- 4

VA.error.g4.linear <- train.svm.kCV.gamma ("linear", myC = 1)
VA.error.g4.poly.2 <- train.svm.kCV.gamma ("poly",degr = 2, myC = 1, myGamma = g4)
VA.error.g4.poly.3 <- train.svm.kCV.gamma ("poly",degr = 3, myC = 1, myGamma = g4)
VA.error.g4.poly.4 <- train.svm.kCV.gamma ("poly",degr = 4, myC = 1, myGamma = g4)
VA.error.g4.poly.5 <- train.svm.kCV.gamma ("poly",degr = 5, myC = 1, myGamma = g4)
VA.error.g4.poly.6 <- train.svm.kCV.gamma ("poly",degr = 6, myC = 1, myGamma = g4)
VA.error.g4.poly.7 <- train.svm.kCV.gamma ("poly",degr = 7, myC = 1, myGamma = g4)
VA.error.g4.RBF <- train.svm.kCV.gamma ("RBF", myC = 1, myGamma = g4)


G.0.0625 <- c(VA.error.g00625.linear,VA.error.g00625.poly.2,VA.error.g00625.poly.3,VA.error.g00625.poly.4,VA.error.g00625.poly.5,VA.error.g00625.poly.6,VA.error.g00625.poly.7,VA.error.g00625.RBF)

G.0.125 <- c(VA.error.g0125.linear,VA.error.g0125.poly.2,VA.error.g0125.poly.3,VA.error.g0125.poly.4,VA.error.g0125.poly.5,VA.error.g0125.poly.6,VA.error.g0125.poly.7,VA.error.g0125.RBF)

G.0.25 <- c(VA.error.g025.linear,VA.error.g025.poly.2,VA.error.g025.poly.3,VA.error.g025.poly.4,VA.error.g025.poly.5,VA.error.g025.poly.6,VA.error.g025.poly.7,VA.error.g025.RBF)

G.0.5 <- c(VA.error.g05.linear,VA.error.g05.poly.2,VA.error.g05.poly.3,VA.error.g05.poly.4,VA.error.g05.poly.5,VA.error.g05.poly.6,VA.error.g05.poly.7,VA.error.g05.RBF)

G.1 <- c(VA.error.g1.linear,VA.error.g1.poly.2,VA.error.g1.poly.3,VA.error.g1.poly.4,VA.error.g1.poly.5,VA.error.g1.poly.6,VA.error.g1.poly.7,VA.error.g1.RBF)

G.2 <- c(VA.error.g2.linear,VA.error.g2.poly.2,VA.error.g2.poly.3,VA.error.g2.poly.4,VA.error.g2.poly.5,VA.error.g2.poly.6,VA.error.g2.poly.7,VA.error.g2.RBF)

G.4 <- c(VA.error.g4.linear,VA.error.g4.poly.2,VA.error.g4.poly.3,VA.error.g4.poly.4,VA.error.g4.poly.5,VA.error.g4.poly.6,VA.error.g4.poly.7,VA.error.g4.RBF)



names <- c("Linear", "Poly 2", "Poly 3", "Poly 4", "Poly 5", "Poly 6", "Poly 7", "RDF")

results <- data.frame(names,G.0.0625,G.0.125,G.0.25,G.0.5,G.1,G.2,G.4)

df <- melt(results ,  id.vars = 'names', variable_name = 'Gammas')

ggplot(df,aes(names,value,group = Gammas, colour = Gammas)) + geom_line()

# Elegimos una gamma de 0.0625, C de 1 y kernel polinomico de grado 2

(model <- svm(dataset[,2:length(dataset)],dataset[,1], type="C-classification", cost=1, kernel="polynomial", degree=2, coef0=1, scale = TRUE, gamma = 0.0625))

# Lo probamos con los datos de test

dataset.test <- read.csv("data.test.csv",header=TRUE)

dataset.test.scaled <- cbind(dataset.test[,1],  data.frame(scale(dataset.test[,2:length(dataset.test)])))

dataset.test <- dataset.test.scaled

pred <- predict(model,dataset.test[,2:length(dataset.test)])
t_true <- dataset.test[,1]

table(pred,t_true)

(sum(pred != t_true)/length(t_true))


