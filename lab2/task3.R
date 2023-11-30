
library(e1071)
library(kernlab)
library(caret)
# Clear the workspace
rm(list = ls())
set.seed(8312)
load('HWD.RData')
X <- as.data.frame(data)
n <- dim(X)[1]

# Assuming the first column contains the target variable
id <- sample(1:n, floor(0.8 * n)) 
training <- X[id, ] 
test <- X[-id, ] 

training$V1 <- as.factor(training$V1)
test$V1 <- as.factor(test$V1)

# Using linear SVM model -> vanilladot
svm_1 <- ksvm(V1 ~ ., data = training, kernel = 'vanilladot',kpar=list(), C = 0.001)

pred <- predict(svm_1, newdata = test[,-1])
conf_matrix <- confusionMatrix(pred, test$V1)
print(conf_matrix$table)
print(conf_matrix$overall)


# Cross validation for the linear kernel

slackness <- c(0.0001, 0.0005, 0.001, 0.005, 0.1, 1)
grid <- expand.grid(C = slackness)

cv <- trainControl(method = "cv",
                   number = 10,
                   verboseIter = TRUE
                   )


# Train the SVM model using the train function
svm <- train(V1 ~ .,
             data = training,
             method = "svmLinear",
             trControl = cv,
             tuneGrid = grid)

# Print the trained model
print(svm)

best_C <- svm$bestTune$C

# Test on true test set
svm_best <- ksvm(V1 ~ ., data = training, kernel = 'vanilladot',kpar=list(), C = best_C)
pred <- predict(svm_best, newdata = test[,-1])
conf_matrix <- confusionMatrix(pred, test$V1)
print(conf_matrix)

#############################################################
#############################################################


# Task 3.2: Do the same but using a nonlinear kernel

slackness <- c(1)
sigmas <- c(0.005, 0.1)
combination = expand.grid(C=slackness, sigma=sigmas)

cv <- trainControl(method = "cv",
                   number = 10,
                   verboseIter = TRUE
)


# Train the SVM model using the train function
svm <- train(V1 ~ .,
             data = training,
             method = "svmRadial",
             trControl = cv,
             tuneGrid = combination)

# Print the trained model
print(svm)

best <- svm$bestTune

# Test on true test set
svm_best <- ksvm(V1 ~ ., data = training, kernel = 'rbfdot',kpar=list(best$sigma), C = best$C)
pred <- predict(svm_best, newdata = test[,-1])
conf_matrix <- confusionMatrix(pred, test$V1)
print(conf_matrix)


