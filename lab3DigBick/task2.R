# Clear the workspace
rm(list = ls())

w1 <- c(0.072, 0.072, 0.071, 0.071, 0.071, 0.167, 0.167, 0.071, 0.167, 0.071)

# Train weak learner
classification <- c(1, 1, 0, 0, 0, 1, 1, 0, 1, 0)
classes <- classification
classes[which(classes == 1)] = 2

# Calculate errors
my_eps <- t(w1)%*%classification
theta <- sqrt(my_eps / (1 - my_eps))

w2 <- numeric(length(w1))
for (i in 1:length(w1)) {
  w2[i] <- w1[i]*theta^(1 - (classes[i]))
}

w2 <- w2 / sum(w2)
print(round(w2, 4))

