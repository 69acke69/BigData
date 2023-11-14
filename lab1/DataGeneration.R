rm(list=ls())
set.seed(201606)
N <- 20
x1 <- runif(N,-1,1)
x2 <- runif(N,-1,1)
X <- cbind(x1,x2)
y <- ifelse(x2>x1,-1,1)

#Plot the two different groups 
colors <- ifelse(y == 1, "blue", "red")
plot(X[, 1], X[, 2], col = colors, pch = 16, main = "Scatter Plot with Different Colors", xlab = "X1", ylab = "X2")

#Add ones
X <- cbind(1,x1,x2)

#Initial guess for weights
w <- c(1,1,1)

max_iter = 100
for(j in 1:max_iter){
  misclassified <- 0
  # Loop over the dataset
  for(i in 1:N){
    
    # Checks for misclassification
    if(sign(w%*%X[i,]) != y[i]) {
      
      # Update weights!! w
      w <- w + y[i]%*% X[i,]
      misclassified <- misclassified + 1
    }
  }
  # Checks if the algorithm has converged
  if(misclassified == 0) {
    cat("Converged in ",j)
    break
  }
}

#Verify that the obtained weights defines a good decision boundary 
x_values <- seq(-1, 1, length.out = 100)
y_values <- (-w[1] - w[2] * x_values) / w[3]
lines(x_values, y_values, col = "green", lwd = 2)
