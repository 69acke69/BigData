# PCA on ellipse pattern observations   #
# Motivating example for kernel methods #

rm(list = ls())
#-------------------------#
#--- generate the data ---#
#-------------------------#
DGP_ellipse <- function(N = 50, seed = 8312){
  set.seed(seed)
  oval_fun <- function(x,a=1,b=0.5){b*sqrt(1-(x/a)^2)}
  x11 = runif(N, -1, 1)
  x12 = c(oval_fun(x11[1:(.5*N)]),-oval_fun(x11[(.5*N+1):N])) + rnorm(N, 0, 0.05)
  X = cbind(x11, x12)
  x21 = runif(N, -1.5, 1.5)
  x22 = c(oval_fun(x21[1:(.5*N)],a=1.5,b=0.75),-oval_fun(x21[(.5*N+1):N],a=1.5,b=0.75)) + rnorm(N, 0, 0.05)
  X = rbind(X, cbind(x21,x22))
  Q = eigen(matrix(c(1,-4,-4,1),2,2))$vectors
  X = X%*%Q
  y = c(rep(1,N), rep(0, N))
  d = cbind(y, X)
  return(d)
}
d = DGP_ellipse()

#----------------#
#--- Vis Data ---#
#----------------#
X = d[,-1]
y = d[,1]
plot(X, pch=20, col = y+2, xlab = "X1", ylab = "X2", asp = 1, cex = 3)

#-------------------#
#--- regular PCA ---#
#-------------------#
n = dim(d)[1]
M = matrix(rep(colMeans(X), n), byrow = T, ncol = 2)
XX = X-M # demean 
# one = matrix(rep(1,n), n, 1)
# XX = (diag(n) - (1/n)*one%*%t(one))%*%X # demean
P = eigen(t(XX)%*%XX)$vectors
#P = eigen(cov(X))$vectors
z = XX%*%P # prinicpal components
plot(z[,1], rep(0,n), pch=20, col = y+2, xlab = "Z1", ylab = "", asp = 1, cex = 3)
plot(z[,2], rep(0,n), pch=20, col = y+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)

#---------------------------------#
#--- non-linear transformation ---#
#---------------------------------#
h1 <- X[,1]^2; h2 <- sqrt(2)*X[,1]*X[,2]; h3 <- X[,2]^2 # feature mapping
library(rgl)
plot3d(h1, h2, h3, col=y+2, size=8)
H = cbind(h1,h2,h3) # augmented data matrix
M_h = matrix(rep(colMeans(H), n), byrow = T, ncol = 3)
HH = H-M_h
P = eigen(t(HH)%*%HH)$vectors
z = HH%*%P
plot(z[,1], rep(0,n), pch=20, col = y+2, xlab = "Z1", ylab = "", asp = 1, cex = 3)
plot(z[,2], rep(0,n), pch=20, col = y+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)
plot(z[,3], rep(0,n), pch=20, col = y+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)

#------------------------------------------------------------------------------------#
#--- Do PCA in the augmented feature space, but don't exactly use feature mapping ---#
#------------------------------------------------------------------------------------#
## define the kernel function 
k_fun = function(x,y){ 
  return((sum(x*y))^2)
}

## calculate the gram matrix
K = matrix(0, n, n)
for(i in 1:n){
  for(j in i:n){
    K[i,j] = k_fun(X[i,],X[j,])
  }
}
K = K + t(K) - diag(diag(K))

## calculate the centralized gram matrix
C = matrix(1/n, n, n)
K = K-C%*%K-K%*%C+C%*%K%*%C
## eigen decomposition
res = eigen(K)
## calculate the principle components
zz = res$vectors
plot(zz[,3], rep(0,n), pch=20, col = y+2, xlab = "Z2", ylab = "", asp = 1, cex = 3)
