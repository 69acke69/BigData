rm(list = ls())

data(Boston)
set.seed(2023)

train_indices <- sample(1:nrow(Boston), 400)

train_data <- Boston[train_indices, ]
test_data <- Boston[-train_indices, ]

trainY <- train_data$medv
trainX <- train_data[, !(names(train_data) %in% c("medv"))]

testY <- test_data$medv
testX <- test_data[,!(names(test_data) %in% c("medv"))]

trainX <- as.matrix(do.call(cbind, trainX))

testX <- as.matrix(do.call(cbind, testX))

rigde_reg <- function(trainX, trainY, lambda) { 
  p <- ncol(trainX)
  I <- diag(p)
  w <- solve(t(trainX) %*% trainX + lambda * I) %*% t(trainX) %*% trainY
  return(w)
}

#Predictions
#Xtest%*%rigde_reg() == ytest

## BÖRJA HÄR MED ATT GÖRA CROSS VALIDATION
tuning_par = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1)



#-----------
# cross validation FRÅN XIJIA jämför och gör en egen använd rigde_reg istället för Lm.rigde!!
tuning_par <- c(0, 0.01, 0.1, 1)
res_mse <- numeric(4)
pre_y <- numeric(5)

for(i in 1:length(tuning_par)){
  for(j in 1:dim(data)[1]){
    temp_model <- lm.ridge(y~., data = data[-j, ], lambda = tuning_par[i])
    pre_y[j] <- sum(as.numeric(coef(temp_model))*c(1,as.numeric(data[j,-1])))
  }
  res_mse[i] <- mean((pre_y - data$y)^2)
}
res_mse



