rm(list = ls())
load('full')

# Set seed for reproducibility
set.seed(2020)

full <- full[complete.cases(full$Survived), , drop = FALSE]

# Randomly select 400 indices for training
train_indices <- sample(1:nrow(full), 800)

# Create training and testing datasets
train<- full[train_indices, ]
test <- full[-train_indices, ]

rf_model <- randomForest(factor(Survived) ~ Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title +
                           FsizeD + Child + Mother,
                         data = train, ntree = 50)



(importance    <- importance(rf_model))

prediction <- predict(rf_model, test)
correct_predictions <- sum(test$Survived == prediction)
total_predictions <- nrow(test)
accuracy <- (correct_predictions / total_predictions) * 100
cat("Accuracy:", accuracy, "%\n")



