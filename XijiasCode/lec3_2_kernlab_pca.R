#--------------------#
# Demo of Kernel PCA #
#--------------------#

set.seed(8312)
N <- 1000
X1 <- runif(N, -1, 1); X2 <- runif(N, -1, 1)
d <- X1^2+X2^2
X <- cbind(X1, X2)[which(d<1 & d>0.5 | d<0.2), ]
label <- numeric(N)
label[which(d<1 & d>0.5)] <- 1
label[which(d<0.2)] <- 2
label <- label[which(label != 0)]
plot(X, col = label, asp = 1, pch = 20, cex = 2)

# Kernel PCA
library(kernlab)
kpc <- kpca(X, kernel = "rbfdot", features = 2, kpar = list(sigma=1))
# res = kpc@pcv # This is the same way we did in 'lec3_1_ellipse_pca' 
res = rotated(kpc) # this is another way to extract the kpca from the results
plot(res, col=label, cex = 2, pch = 20) 


sig <- seq(0.5, 6, 0.1)
for(i in 1:length(sig)){
  kpc <- kpca(X, kernel = "rbfdot", features = 2, kpar = list(sigma=sig[i]))
  plot(rotated(kpc), col=label, main = paste("Sigma=", sig[i]), cex = 2, pch = 20)
  Sys.sleep(0.25)
}
