# Task 1: Kernel Ridge Regression using Gaussian Kernel

# Load necessary libraries
library(MASS)
library(kernlab)

# Clear the workspace
rm(list = ls())

# Load the Boston dataset
X = data(Boston)

# Define the Gaussian kernel function
kernel_function <- function(x1, x2, sigma) {
  result <- exp(-sqrt(sum((x1 - x2)^2)) / (2 * sigma^2))
  return(result)
}

# Function to calculate the Gram matrix
calculate_gram_matrix <- function(X, sigma) {
  n <- nrow(X)
  gram_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      gram_matrix[i, j] <- kernel_function(X[i,], X[j,], sigma)
    }
  }
  return(gram_matrix)
}

# Function to predict using Kernel Ridge Regression
predictKRR <- function(K, trainX, trainY, X, lambda, sigma) {
  # Calculate kappa-vector
  N = dim(K)[1]
  kappa <- matrix(0, nrow(K), 1)
  for (i in 1:N) {
    kappa[i] = kernel_function(trainX[i,], X, sigma)
  }
  
  # Predict y_hat
  y_hat = trainY %*% solve(K + diag(lambda, nrow(K))) %*% kappa
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
testX <- test_data[, !(names(test_data) %in% c("medv"))]
trainX <- as.matrix(trainX)
testX <- as.matrix(testX)

# Task 2: Kernel Ridge Regression with specified lambda and sigma
lambda = 0.1
sigma = 0.001

# Calculate the Gram matrix
K = calculate_gram_matrix(trainX, sigma)

# Initialize prediction vector
pred <- matrix(0, length(testY), 1)

# Make predictions for each test data point
for (k in 1:length(testY)) {
  pred[k] = predictKRR(K, trainX, trainY, testX[k,], lambda, sigma)
}

# Calculate and print the root mean squared error (RMSE) on the test data
rmse = sqrt(mean((pred - testY)^2))
cat("RMSE on test data:", rmse, "\n")



# Task 3: Implementing k-fold cross-validation for kernel ridge regression

# Function for doing k-fold cross-validation
k_fold <- function(trainX, trainY, k, lambda, sigma) {
  
  # Shuffle indices to create random folds
  order <- sample(1:nrow(trainX))
  
  # Calculate the number of points in each fold
  num_points <- round(nrow(trainX) / k)
  
  # Initialize vector to store RMSE values for each fold
  rmse <- numeric(k)
  
  for (fold in 0:(k - 1)) {
    # Determine the start and end indices for the current fold
    start_ind = fold * num_points + 1
    end_ind <- (fold + 1) * num_points
    
    # Extract indices for the testing set
    testing_idx <- order[start_ind:end_ind]
    
    # Split the dataset into training and testing sets
    test_setX <- trainX[testing_idx,]
    training_setX <- trainX[-testing_idx,]
    
    test_setY <- trainY[testing_idx]
    training_setY <- trainY[-testing_idx]
    
    # Evaluate the model
    K = calculate_gram_matrix(training_setX, sigma)
    pred <- matrix(0, dim(test_setX)[1], 1)
    for (i in 1:dim(test_setX)[1]) {
      pred[i] = predictKRR(K, training_setX, training_setY, test_setX[i,], lambda, sigma)
    }
    
    # Calculate the root mean squared error for the current fold
    rmse[fold] <- sqrt(mean((pred - test_setY)^2))
  }
  return(rmse)
}

## Normalize --------------------------

# Load the Boston dataset and normalize the features
data(Boston)
X <- scale(as.matrix(Boston))

# Set seed for reproducibility
set.seed(2020)

# Randomly select 400 indices for training
train_indices <- sample(1:nrow(X), 400)

# Create training and testing datasets
train_data <- X[train_indices, ]
test_data <- X[-train_indices, ]

# Extract target variables and feature matrices for training and testing
trainY <- train_data[,14]
trainX <- train_data[,1:13]
testY <- test_data[,14]
testX <- test_data[,1:13]

# -------------------------------

# Define a grid of hyperparameters (lambda and sigma)
lambda_values = c(0.1, 0.01, 0.001)
sigma_values = c(0.1, 0.01, 0.001)

# Generate all combinations of hyperparameters
comb = expand.grid(lambda_values, sigma_values)

# Initialize a matrix to store RMSE values for each combination
rmse_comb = matrix(0, nrow(comb), 1)

# Perform k-fold cross-validation for each hyperparameter combination
for (i in 1:nrow(comb)) {
  cat(i, "/", nrow(comb), "\r")
  
  # Call the k_fold function to get RMSE values for the current combination
  rmse <- k_fold(trainX, trainY, 10, comb[i,1], comb[i,2])
  
  # Store the average RMSE for the current combination
  rmse_comb[i] <- mean(rmse)
}

# Plot the RMSE values for each combination
plot(rmse_comb)

# Identify the index of the combination with the lowest RMSE
idx <- which.min(rmse_comb)

# Print the best combination of hyperparameters
cat("Best combination: lambda =", comb[idx,1], ", sigma =", comb[idx,2], "\n")

# Test the model with the best hyperparameters on the test data
pred <- matrix(0, length(testY), 1)
K = calculate_gram_matrix(trainX, comb[idx,2])
for (k in 1:length(testY)) {
  pred[k] = predictKRR(K, trainX, trainY, testX[k,], comb[idx,1], comb[idx,2])
}

# Calculate and print the RMSE on the test data
rmse = sqrt(mean((pred - testY)^2))
cat("RMSE on test data:", rmse, "\n")
