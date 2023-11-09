rm(list=ls())

set.seed(201606)
N <- 20
x1 <- runif(N,-1,1)
x2 <- runif(N,-1,1)
X <- cbind(x1,x2)
y <- ifelse(x2>x1,-1,1)


xdata <- seq(-1, 1, length = 100)
w = c(1,1)
f = function(x) {-w[1]/w[2]*x}

plot(xdata, f(xdata), type='l')
arrows(0,0,w[1]/5,w[2]/5)

points(x1,x2)



for(i in 1:N) {
  yhat = w%*%X[i,]
  w = w + sign(yhat)%*%X[i,]
  
  plot(xdata, f(xdata), type='l')
  arrows(0,0,w[1]/5,w[2]/5)
  
} 

arrows(0,0,w[1],w[2])
