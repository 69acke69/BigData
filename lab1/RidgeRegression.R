rm(list = ls())
library(MASS)
data(Boston)
set.seed(2023)

# Prepare the data in training and test sets
train_indices <- sample(1:nrow(Boston), 400)

train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]

trainY <- train_data$medv
trainX <- train_data[, !(names(train_data) %in% c("medv"))]

testY <- test_data$medv
testX <- test_data[,!(names(test_data) %in% c("medv"))]

trainX <- as.matrix(trainX)
testX <- as.matrix(testX)

# Solve the LS problem for the optimal weights
ridge_reg <- function(trainX, trainY, lambda) { 
  p <- ncol(trainX)
  I <- diag(p)
  w <- solve(t(trainX) %*% trainX + lambda * I) %*% t(trainX) %*% trainY
  return(w)
}

# Function for doing k-fold cross-validation
k_fold <- function(trainX, trainY, k) {
  
  order <- sample(1:nrow(trainX))
  num_points <- round(nrow(trainX)/k)
  rmse <- numeric(k)
  
  for (fold in 0:(k-1)) {
    # Determine the amount of folds to use
    start_ind = fold*num_points+1
    end_ind <- (fold+1)*num_points
    
    testing_idx <- order[start_ind:end_ind]
    
    # Obtain the sets of features
    test_setX <- trainX[testing_idx,]
    training_setX <- trainX[-testing_idx,]
    
    # Obtain the sets of targets
    test_setY <- trainY[testing_idx]
    training_setY <- trainY[-testing_idx]
    
    # Evaluate the model
    predictions <- test_setX%*%ridge_reg(training_setX, training_setY, tuning_param[i])
    # Calculate the root mean squared error
    rmse[fold] <- sqrt(mean((predictions - test_setY)^2))
  }
  return(rmse)
}

# Set the tuning parameters to loop through
tuning_param = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1)
tuning_result_rmse <- numeric(length(tuning_param))

# Loop over the tuning parameters
for (i in 1:length(tuning_param)) {
  rmse <- k_fold(trainX, trainY, 10)
  tuning_result_rmse[i] <- mean(rmse)
}

# Show the result
plot(c(tuning_param),tuning_result_rmse,
     pch=0,col="red",cex=1,lwd=2,asp=1,
     xlab="Tuning parameter",ylab="RMSE average")
best_res <- which.min(tuning_result_rmse)
points(tuning_param[best_res], tuning_result_rmse[best_res], pch=1, lwd = 2, cex = 3)
# Choose best parameter
best_lambda <- tuning_param[best_res]
print(best_lambda)

# Estimate the model performance on the actual testing set
w_reg <- ridge_reg(trainX, trainY, best_lambda)
predictions <- testX%*%w_reg
rmse <- sqrt(mean((predictions - testY)^2))
cat("The performance of the model using the best lambda is:", rmse, "\n")
