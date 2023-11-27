#---------------------------------#
#--- Demo of package 'kernlab' ---#
#------ Xijia Liu 2019 Nov -------#
#---------------------------------#

rm(list=ls())

library(kernlab)

#? ksvm

data <- iris[51:150,3:5]

#levels(data$Species)

data$Species <- as.factor(as.numeric(data$Species))
levels(data$Species) <- c("versicolor", "virgincia")

# vis

plot(data$Petal.Length, data$Petal.Width, col = data$Species, asp = 1)

# Fit a linear soft svm, i.e. linear kernel function

(svm_1 <- ksvm(Species~., data = data, kernel = 'vanilladot', kpar=list(), C = 1))

(acc <- mean(predict(svm_1, data) == data$Species))

plot(svm_1, data = data)

# Fit a RBF (gaussian) kernel svm

(svm_2 <- ksvm(Species~., data = data, kernel = 'rbfdot', kpar=list(sigma = 1)))

plot(svm_2, data = data)

(svm_2 <- ksvm(Species~., data = data, kernel = 'rbfdot', kpar=list(sigma = 10)))

plot(svm_2, data = data)

# Cross validation error

(svm_3 <- ksvm(Species~., data = data, kernel = 'rbfdot', kpar=list(sigma = 1), cross = 5))
(svm_3 <- ksvm(Species~., data = data, kernel = 'rbfdot', kpar=list(sigma = 100), cross = 5))

# some notes:
# 1) SVM can be applid for a regression problem. If you want to train a svm classifier, then the 
# target variable shuold be a factor type, otherwise, you have to choose type = 'C-svc'
# 2) If y is a continuous variable, then tube regression will be done. 'epsilon' is the sensitive parameter
# 3) When you use 'rbfdot', if you do not fix the value for sigma, then computer will estimate a 
# reasonable value for it.
# 4) SVM also provide the probability type prediction. 'prob.model' = T
