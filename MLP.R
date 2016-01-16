#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
## MLP (nnet)                                      ##
#####################################################
library(MASS)
library(nnet)

# Leemos los datos ya separados en train y test
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)
    
# Split data
set.seed(42)

learn <- sample(1:N, round(2*N/3))

nlearn <- length(learn)
ntest <- N - nlearn

# Escalando los datos
for (i in 2:length(data.clean)) {
  data.clean[,i] <- scale(data.clean[,i])
}

# Generando el modelo con 2 neuronas por probar algo
model.nnet <- nnet(diagnosis ~., data = data.clean, subset=learn, size=2, maxit=200, decay=0)

# Vamos a ver el error de train
p1 <- as.factor(predict (model.nnet, type = "class"))
t1 <- table(p1,data.clean$diagnosis[learn])
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

## OMG! 0? IMPOSSIBRU OVERFITTING 4 SURE

# Vamos a ver el error de test
p2 <- as.factor(predict (model.nnet, newdata=data.clean[-learn,], type="class"))
t2 <- table(p2,data.clean$diagnosis[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## -xxx%?
################################## HASTA AQUÍ ################################################### 





## We get 26.32%, so it seems that the MLP helps a little bit; however, we need to work harder

## We are going to do the modelling in a principled way now. Using 10x10 CV to select the best
## combination of 'size' and 'decay'

## Just by curiosity, let me show you that we can fit any dataset (in the sense of reducing the training error):

model.nnet <- nnet(diagnosis ~., data = data.train, size=20, maxit=200)

# Now let's compute the training error

p1 <- as.factor(predict (model.nnet, type="raw"))
length(data.test[,1])
(t1 <- table(p1,data.train$diagnosis))
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

# And the corresponding test error

p2 <- as.factor(predict (model.nnet, newdata=json_data[-learn,], type="raw"))

(t2 <- table(p2,json_data$Num_sprints[-learn]))
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## That's it: we got a training error around 6% (four times lower than the previous one), but it is 
## illusory ... the test error is larger than before (around 40%); 
## actually the relevant comparison is between 6% and 40%, this large gap is an indication of overfitting


## {caret} is an excellent package for training control, once you know what all these concepts are

## WARNING: if the package is not installed in your computer, installation needs some previous packages
library(caret)

## For a specific model, in our case the neural network, the function train() in {caret} uses a "grid" of model parameters
## and trains using a given resampling method (in our case we will be using 10x10 CV). All combinations are evaluated, and 
## the best one (according to 10x10 CV) is chosen and used to construct a final model, which is refit using the whole training set

## Thus train() returns the constructed model (exactly as a direct call to nnet() would)

## In order to find the best network architecture, we are going to explore two methods:

## a) Explore different numbers of hidden units in one hidden layer, with no regularization
## b) Fix a large number of hidden units in one hidden layer, and explore different regularization values (recommended)

## doing both (explore different numbers of hidden units AND regularization values) is usually a waste of computing 
## resources (but notice that train() would admit it)

## Let's start with a)

## set desired sizes

(sizes <- 2*seq(1,10,by=1))

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10)

model.10x10CV <- train (Num_sprints ~., data = json_data, subset=learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc)

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

## The results are quite disappointing ...

## Now method b)

(decays <- 10^seq(-3,0,by=0.1))

## WARNING: this takes a few minutes
model.10x10CV <- train (Num_sprints ~., data = json_data, subset=learn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=20,.decay=decays), trControl=trc)

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune

## The results are a bit better; we should choose the model with the lowest 10x10CV error overall,
## in this case it corresponds to 20 hidden neurons, with a decay of 0.7943282

## So what remains is to predict the test set with our final model

p2 <- as.factor(predict (model.10x10CV, newdata=json_data[-learn,], type="raw"))

t2 <- table(pred=p2,truth=json_data$Num_sprints[-learn])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

## We get 27.82% after all this work; it seems that the information in this dataset is not enough
## to accurately predict admittance. Note that ...

## ... upon looking at the confusion matrix for the predictions ...
t2

## it clearly suggests that quite a lot of people is getting accepted when they should not, given their gre, gpa and rank
## It is very likely that other (subjective?) factors are being taken into account, that are not in the dataset

## A different approach is to do the same thing but using the square error instead of the cross-entropy
## This is conceptually different, because we are now treating the class labels as numbers
## However, in practice it often delivers similar results
