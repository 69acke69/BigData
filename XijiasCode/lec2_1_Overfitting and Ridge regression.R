# about ridge regression
rm(list = ls())

set.seed(2016)
t <- seq(0,6,0.1)
x <- c(1,3,4,4.8,5)
f <- function(x){0.5*(x-2)*(x-4.5)}
y <- f(x) + rnorm(length(x),0,0.2)

plot(x,y,pch=4,col="red",cex=2,lwd=2,asp=1,xlab="X",ylab="Y",xlim=c(0.5,5.5),ylim=c(-1,4))

points(t,f(t),type="l",col="blue",lwd=2,lty=2)

m1 <- lm(y ~ x+I(x^2)); 
coef1 <- as.numeric(m1$coefficients)
f1 <- function(x){coef1[1]+coef1[2]*x+coef1[3]*x^2}
points(t,f1(t),type="l",col="red",lwd=2)

m2 <- lm(y ~ x+I(x^2)+I(x^3)+I(x^4))
coef2 <- as.numeric(m2$coefficients)
f2 <- function(x){coef2[1]+coef2[2]*x+coef2[3]*x^2+coef2[4]*x^3+coef2[5]*x^4}
points(t,f2(t),type="l",col="orange",lwd=2)

# Ridge regression

library(MASS)

data <- data.frame(cbind(y,x,x^2,x^3,x^4))

plot(x,y,pch=4,col="black",cex=2,lwd=2,asp=1,xlab="X",ylab="Y",xlim=c(0.5,5.5),ylim=c(-1,4))

(coef1 <- coef(lm.ridge(y~., data, lambda=0)))
f <- function(x,coef){coef[1]+coef[2]*x+coef[3]*x^2+coef[4]*x^3+coef[5]*x^4}

points(t,f(t,coef1),type="l",col="orange")

(coef2 <- coef(lm.ridge(y~., data, lambda=0.001)))
points(t,f(t,coef2),type="l",col="red")

(coef3 <- coef(lm.ridge(y~., data, lambda=0.01)))
points(t,f(t,coef3),type="l",col="purple")

(coef4 <- coef(lm.ridge(y~., data, lambda=1)))
points(t,f(t,coef4),type="l",col="green")

# cross validation
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

#-----------------------------------------------------------------------------------------------------#

library(glmnet)

X = as.matrix(data[,-1]) # input X in cv.glmnet must be matrix, but not data frame

tuning_par = c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1)
m <- cv.glmnet(x = X, y = y, lambda = tuning_par, nfolds = 5, alpha = 0, grouped = F, maxit = 500000)
m$lambda.min
coef(m, s = "lambda.min")

# Ridge
m = glmnet(x = X, y = y, lambda = m$lambda.min, alpha = 0, maxit = 500000)
coef(m)

m = glmnet(x = X, y = y, lambda = tuning_par, alpha = 0, maxit = 500000)
coef(m, s = 0.0001)

points(t,f(t, coef(m, s = 0.0001)),type="l",col="brown",lwd=5)

# LASSO
m <- cv.glmnet(x = X, y = y, lambda = tuning_par, nfolds = 5, alpha = 1, grouped = F, maxit = 500000)
coef(m, s = "lambda.min")
points(t,f(t, coef(m, s = "lambda.min")),type="l",col="black",lwd=5)

#-----------------------------------------------------------------------------------------------------#