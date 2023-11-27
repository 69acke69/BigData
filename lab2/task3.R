install.packages("e1071")
library(e1071)
library(kernlab)
# Clear the workspace
rm(list = ls())

load('HWD.RData')
X <- as.data.frame(data)
n <- dim(X)[1]

# Assuming the first column contains the target variable
id <- sample(1:n, floor(0.8 * n)) 
training <- X[id, ] 
test <- X[-id, ] 

training$V1 <- as.factor(training$V1)
test$V1 <- as.factor(test$V1)

svm_1 <- ksvm(V1 ~ ., data = training, kernel = 'vanilladot',kpar=list(), C = 0.001)

pred <- predict(svm_1, newdata = test[,-1])

# Convert probabilities to integer predictions
pred_int <- as.integer(factor(pred, levels = levels(test$V1)))

# Evaluate accuracy
accuracy <- sum(pred_int == as.integer(test$V1)) / length(test$V1)
print(paste("Accuracy:", accuracy))

### --------- TROR ATT DENNA GÖRA RÄTT! INTE SÄKER! börja kolla på cross validation 









