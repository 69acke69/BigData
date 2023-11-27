# the way you visualize the images of digit

colors <- c('white','black'); cus_col <- colorRampPalette(colors=colors)

z <- matrix(as.numeric(train[1,257:2]),16,16,byrow=T)[,16:1]
image(t(z),col=cus_col(256))

par(mfrow = c(5,5))
set.seed(2018)
for(i in sample(1:dim(train)[1], 25)){
  z <- matrix(as.numeric(train[i,257:2]),16,16,byrow=T)[,16:1]
  image(t(z),col=cus_col(256))
}

