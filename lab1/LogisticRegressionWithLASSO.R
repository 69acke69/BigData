rm(list = ls())
library(MASS)
library(glmnet)
library(caret)
set.seed(2023)

# ----- Loading ----- #
# Load the data from files
gene_data <- read.table("GeneExpressionData.txt")
meta_data <- read.table("MetaData.txt")

# Draw 181 random samples from the gene data
random_idx <- sample(1:ncol(gene_data), 181)

# ----- Feature data ----- #

# Training data
train_dataX <- as.matrix(gene_data[2:nrow(gene_data),random_idx])
train_dataX <- apply(train_dataX, c(1, 2), as.numeric)

# Testing data
test_dataX <- as.matrix(gene_data[2:nrow(gene_data),-random_idx])
test_dataX <- apply(test_dataX, c(1, 2), as.numeric)


# ----- Targets ----- #

# Training data
text_train_dataY <- meta_data$V31[random_idx]
train_dataY <- ifelse(text_train_dataY == "IDHmut-non-codel", 0, 1)

# Testing data
text_test_dataY <- meta_data$V31[-random_idx]
test_dataY <- ifelse(text_test_dataY == "IDHmut-non-codel", 0, 1)

# ----- Training ----- #

# Train the model
model <- cv.glmnet(x = t(train_dataX), 
                y = train_dataY,
                family = "binomial",
                type.measure = "class",
                alpha = 1,
                nfolds = 10)

# ----- Evaluation ----- #
# Show the plot over different lambdas
plot(model)

# Make predictions on the test set
predictions <- as.numeric(predict(model, t(test_dataX), s = "lambda.min", type = "class"))
# Evaluate the final model
confusionMatrix(as.factor(predictions), as.factor(test_dataY))

