library(MASS)
library(kernlab)

rm(list = ls())

X = data(Boston)

kernel_function <- function(x1, x2, sigma) {
  result <- exp(-sqrt(sum((x1-x2)^2)) / 2*sigma^2)
  return(result)
}

# Here we generate the Gram matrix
calculate_gram_matrix <- function(X,sigma) {
  n <- nrow(X)
  gram_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      gram_matrix[i, j] <- kernel_function(X[i,], X[j,], sigma)
    }
  }
  return(gram_matrix)
}

predictKRR <- function(K,trainX, trainY, X, lambda, sigma) {
  # Calc K
  # Calc kappa-vector
  N = dim(K)[1]
  kappa <- matrix(0, nrow(K), 1)
  for (i in 1:N) {
    kappa[i] = kernel_function(trainX[i,],X,sigma)
  }
  # predict y_hat
  y_hat = trainY%*%solve(K + diag(lambda, nrow(K)))%*%kappa
  return(y_hat)
}
# Divide into training and test sets
set.seed(2020)
train_indices <- sample(1:nrow(Boston), 400)
train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]
trainY <- train_data$medv
trainX <- train_data[, !(names(train_data) %in% c("medv"))]
testY <- test_data$medv
testX <- test_data[,!(names(test_data) %in% c("medv"))]
trainX <- as.matrix(trainX)
testX <- as.matrix(testX)


#Task 2!!!!
lambda = 0.1
sigma = 0.001
K = calculate_gram_matrix(trainX,sigma)
pred <- matrix(0, length(testY), 1)
for (k in 1:length(testY)) {
  pred[k] = predictKRR(K,trainX,trainY,testX[k,],lambda,sigma)
}
rmse = sqrt(mean((pred-testY)^2))

#Task 3!!!!
## Normalize --------------------------
X = data(Boston)

means <- colMeans(trainX)
sds <- apply(trainX, 2, sd)

# Normalize the training set
trainX <- sweep(trainX, 2, means, "-")
trainX <- sweep(trainX, 2, sds, "/")

# Normalize the test set using the mean and standard deviation from the training set
testX <- sweep(testX, 2, means, "-")
testX <- sweep(testX, 2, sds, "/")

# -------------------------------
lambda = c(0.1,0.01,0.001)
sigma = c(0.1,0.01,0.001)

comb = expand.grid(lambda,sigma)
rmse = matrix(0, nrow(comb), 1)
for(i in 1:nrow(comb)){
  K = calculate_gram_matrix(trainX,comb[i,2])
  pred <- matrix(0, dim(testX)[1], 1)
  for (k in 1:dim(testX)[1]) {
    pred[k] = predictKRR(K,trainX,trainY,testX[k,],comb[i,1],comb[i,2])
  }
  rmse[i] = sqrt(mean((pred-testY)^2))
}
plot(rmse)
#Best combination
comb[3,]
