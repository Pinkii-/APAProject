library(e1071)
setwd("~/Documents/APAProject")
dataset <- read.csv("data.train3.csv",header=TRUE)

summary(dataset)

(model <- svm(dataset[,2:length(dataset)],dataset[,1], type="C-classification", cost=1, kernel="linear", scale = FALSE))

N = lengths(dataset)[1]

k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

train.svm.kCV <- function (which.kernel, myC, kCV=10)
{
  for (i in 1:kCV) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,2:length(dataset)]
    t_train <- train[,1]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
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


C <- 10

#LOOCV
#k <- N 
#folds <- sample(rep(1:k, length=N), N, replace=FALSE)

VA.error.linear <- train.svm.kCV ("linear", myC=C,k)

VA.error.poly.2 <- train.svm.kCV ("poly.2", myC=C)

VA.error.poly.3 <- train.svm.kCV ("poly.3", myC=C)

VA.error.RBF <- train.svm.kCV ("RBF", myC=C)

C
VA.error.linear
VA.error.poly.2
VA.error.poly.3
VA.error.RBF

# Con el data3 y C=10, el linear da 6.60
# Con el data y C=1, el linear da 5.54

dataset.test <- read.csv("data.test3.csv",header=TRUE)

(model <- svm(dataset[,2:length(dataset)],dataset[,1], type="C-classification", cost=10, kernel="linear", scale = FALSE))

pred <- predict(model,dataset.test[,2:length(dataset)])
t_true <- dataset.test[,1]

table(pred,t_true)

(sum(pred != t_true)/length(t_true))
