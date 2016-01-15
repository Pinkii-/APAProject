set.seed(42) # Definimos una seed para poder reproducir la ejecucion

library(e1071)
setwd("~/Documents/APAProject")
dataset <- read.csv("data.train.csv",header=TRUE)

summary(dataset)


N = lengths(dataset)[1]

k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

train.svm.kCV <- function (which.kernel, myC, kCV=10, degr)
{
  for (i in 1:kCV) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,2:length(dataset)]
    t_train <- train[,1]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="linear", scale = FALSE)},
           poly={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=degr, coef0=1, scale = FALSE)},
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="radial", scale = FALSE)},
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



#printamos

C0001

VA.error.c0001.linear
VA.error.c0001.poly.2
VA.error.c0001.poly.3
VA.error.c0001.poly.4
VA.error.c0001.poly.5
VA.error.c0001.poly.6
VA.error.c0001.poly.7
VA.error.c0001.RBF


C001

VA.error.c001.linear
VA.error.c001.poly.2
VA.error.c001.poly.3
VA.error.c001.poly.4
VA.error.c001.poly.5
VA.error.c001.poly.6
VA.error.c001.poly.7
VA.error.c001.RBF

C01

VA.error.c01.linear
VA.error.c01.poly.2
VA.error.c01.poly.3
VA.error.c01.poly.4
VA.error.c01.poly.5
VA.error.c01.poly.6
VA.error.c01.poly.7
VA.error.c01.RBF

C1

VA.error.c1.linear
VA.error.c1.poly.2
VA.error.c1.poly.3
VA.error.c1.poly.4
VA.error.c1.poly.5
VA.error.c1.poly.6
VA.error.c1.poly.7
VA.error.c1.RBF

C10

VA.error.c10.linear
VA.error.c10.poly.2
VA.error.c10.poly.3
VA.error.c10.poly.4
VA.error.c10.poly.5
VA.error.c10.poly.6
VA.error.c10.poly.7
VA.error.c10.RBF

C100

VA.error.c100.linear
VA.error.c100.poly.2
VA.error.c100.poly.3
VA.error.c100.poly.4
VA.error.c100.poly.5
VA.error.c100.poly.6
VA.error.c100.poly.7
VA.error.c100.RBF

C1000

VA.error.c1000.linear
VA.error.c1000.poly.2
VA.error.c1000.poly.3
VA.error.c1000.poly.4
VA.error.c1000.poly.5
VA.error.c1000.poly.6
VA.error.c1000.poly.7
VA.error.c1000.RBF







# Con el data3 y C=10, el linear da 6.60
# Con el data y C=1, el linear da 5.54

dataset.test <- read.csv("data.test3.csv",header=TRUE)

(model <- svm(dataset[,2:length(dataset)],dataset[,1], type="C-classification", cost=10, kernel="linear", scale = FALSE))

pred <- predict(model,dataset.test[,2:length(dataset)])
t_true <- dataset.test[,1]

table(pred,t_true)

(sum(pred != t_true)/length(t_true))


