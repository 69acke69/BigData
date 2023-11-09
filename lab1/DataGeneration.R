rm(list=ls())

set.seed(201606)
N <- 20
x1 <- runif(N,-1,1)
x2 <- runif(N,-1,1)
X <- cbind(x1,x2)
y <- ifelse(x2>x1,-1,1)

plot(x1,x2)

w = c(1,0)

for(i in 1:N) {
  yhat = w%*%X[i,]
  w = w + sign(yhat)%*%X[i,]
} 

arrows(0,0,w[1],w[2])
