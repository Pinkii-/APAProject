#####################################################
## Breast Cancer Wisconsin (Diagnostic) Data Set   ##
##  LDA/QDA/NaiveBayes/kNN                         ##
## version of January, 2016                        ## 
#####################################################

library(MASS) 

# Set working directory
data.clean <- read.csv(file = "data.clean.csv", header = TRUE)
summary(data.clean)

# Convertirmos los datos en dataframes
data.clean <- as.data.frame(data.clean)

# pairs(data.clean, main="Breast Cancer Wisconsin", pch=22, 
#      bg=c("red", "green")[unclass(data.clean$clase)])

#######
# LDA #
#######

## Now we assume gaussianity with equal covariance matrices and use LDA:
## Note the priors can be changed at will; if unspecified, estimates
## from the supplied data will be used

# Separamos aleatoriamente en dos conjuntos. Conjunto de test y conjunto de entrenamiento.
set.seed(43)
learn <- sample(1:nrow(data.clean), round(2*nrow(data.clean)/3))
data.train <- data.clean[learn,]
data.test <- data.clean[-learn,]

data.lda <- lda(clase ~ ., prior = c(1,1)/2, data = data.train)

### LDA can be used for dimension reduction by plotting the data as given by the discriminant functions; these are the a_k(x) as seen in class

## lda prints discriminant functions based on centered (but not standardized) variables
## The "proportion of trace" that is printed is the proportion of between-class variance that is explained by successive discriminant functions
## the list element 'class' gives the predicted class 
## The list element 'posterior' holds posterior probabilities  

data.lda 

# Now we project the data in two new axes 
# (scatter plot using the two discriminant dimensions)
# These new dimensions are linear combinations of the original 4 measurements

plot(data.lda)

# We can explicitly compute the projected data (the new coordinates)
# To illustrate, we just compute projections onto the first LD

# for LDA, this amounts to y = w_1^T x, where w_1 is LD1 in R
d1LDA <- data.train$espesor * data.lda$scaling[1] + data.train$uniformidad_tamano * data.lda$scaling[2] +
        data.train$uniformidad_forma * data.lda$scaling[3] + data.train$adesion_marginal * data.lda$scaling[4] +
        data.train$tamano_celula_unica * data.lda$scaling[5] + data.train$nucleos_bare * data.lda$scaling[6] +
        data.train$cromatina * data.lda$scaling[7] + data.train$nucleos_normales * data.lda$scaling[8] +
        data.train$mitosis * data.lda$scaling[9]
plot(d1LDA)

# which corresponds to the LD1 axis on the previous plot

### PART2: Using LDA for classification

## comparing LDA's prediction with the targets in the data, we get the apparent (a.k.a. training) error (optimistic!)

predict(data.lda, data.train)$class

## we may also have a look at

predict(data.lda, data.train)$posterior # the posterior probabilities

## Assessment of predictive accuracy via LOOCV (leave-one-out cross-validation)
# Some form of cross-validation is necessary if we want to select one model against others

# The idea is to extract one example, build a model with the remaining N-1 and make the model predict the left out example.
# Then change the example and repeat analogously for all of them. As you can see, this process creates N validation sets of size 1.
# The reported result is the fraction of errors (wrongly predicted examples)

# If CV=TRUE, the method returns results (classes and posterior probabilities) for LOOCV.
# Note that if the prior is estimated, the proportions in the whole dataset are used.

data.lda.cv <- lda(clase ~ ., prior = c(1,1)/2, data = data.train, CV=TRUE) 
summary(data.lda.cv$class)

tab <- table(data.clean$clase[learn], data.lda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# If a formula is given as the principal argument the object may be modified using update()
# Example: omit Petal.W from the model and re-evaluate

data.lda.cv.benigno <- update(data.lda.cv, . ~ . - Benigno)

tab <- table(data.clean$clase[learn], data.lda.cv.benigno$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

## Now switch to QDA

data.qda.cv <- qda(clase ~ ., prior = c(1,1)/2, data = data.train, CV=TRUE) 
summary(data.qda.cv$class)

tab <- table(data.clean$clase[learn], data.qda.cv$class)  
(error.LOOCV <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))

# El error para LDA es menor que para QDA y por lo tanto es que le vamos a usar

## Since the LOOCV error for LDA and QDA are equal, we select LDA because it is the less complex, 
## and refit it in the whole training set (that is, no LOOCV)
## In general, we should choose the model with smaller LOOCV

data.lda <- lda(clase ~ ., prior = c(1,1)/2, data = data.train)

## and use it to predict the test set (we get an estimation of true error)

## Now create the predictions in the test set

lda.predictions <- predict(data.lda, data.test)
lda.predictions$class

## and compute the error

tab <- table(data.clean$clase[-learn], lda.predictions$class)  
(error.TE <- 100*(1-sum(tab[row(tab)==col(tab)])/sum(tab)))


####################################################################
## Example 2: Visualizing and classifying wines with LDA and QDA
####################################################################