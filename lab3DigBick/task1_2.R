# Clear the workspace
rm(list = ls())

# Load the relevant packages
library(randomForest)
library(caret)

set.seed(721)

# Load the data set
load("full")
df <- as.data.frame(full)

# Clean up the data set, remove Na values

# Remove columns from data set
columns_to_remove <- c("Deck", "PassengerId", "Ticket", "Cabin", "Surname", "Family", "Child")
df <- df[, -which(names(df) %in% columns_to_remove)]

# Remove rows with Survived NA values
df <- df[-which(is.na(df$Survived)), ]
df$Survived <- as.factor(df$Survived)


# Check if df has NAs
print(any(is.na(df)))

# Train test split
passengers <- nrow(df)
id <- sample(1:passengers, 0.8*passengers)
train <- df[id, ]
test <- df[-id, ]

# Train the actual model
model <- randomForest(Survived~., data = train, 
                       ntree = 100, importance = T)


# Try on test set
predictions <- predict(model, test)
cm <- confusionMatrix(predictions, test$Survived)
print(cm)

# Show the importance of each feature
round(model$importance, 3)

