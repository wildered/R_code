library(mvtnorm)

dat <- read.table("magfield.txt", sep = ",")

x <- dat$V1
y <- dat$V2

dotchart(x, y, col=c('blue', 'red'))

print(t.test(x,y))

print(median(x)-median(y))

sigma = matrix(data=c(10, 50, 50, 10), nrow=2, ncol=2)

mu <- seq(-5,5,length.out=1000)
mvtnorm::rmvnorm(25, mean=rep(c(2),2), sigma = sigma)

