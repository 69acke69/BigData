rm(list = ls())
library(MASS)
library(kernlab)

X = data(Boston)



predictKRR <- function(trainX, trainY, X, lambda, sigma) {
  # Calc K
  rbf <- rbfdot(sigma=sigma)
  K <- kernelMatrix(rbf, trainX)
  
  # Calc kappa-vector
  kappa <- matrix(0, nrow(K@.Data), 1)
  for (i in 1:nrow(K@.Data)) {
    kappa[i] = kernelMatrix(rbf, trainX[i,], X)
  }
  
  # predict y_hat
  y_hat = t(trainY)%*%solve(K@.Data + diag(lambda, nrow(K@.Data)))%*%t(kappa)
  
  return(y_hat)
  
}

# Divide into training and test sets
train_indices <- sample(1:nrow(Boston), 400)
train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]
trainY <- train_data$medv
trainX <- train_data[, !(names(train_data) %in% c("medv"))]
testY <- test_data$medv
testX <- test_data[,!(names(test_data) %in% c("medv"))]
trainX <- as.matrix(trainX)
testX <- as.matrix(testX)


# y_hat = predictKRR(trainX, trainY, testX[1,], 1, 0.05)


