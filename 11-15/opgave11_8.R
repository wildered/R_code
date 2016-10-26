dat <- scan('brain.txt')
mat <- matrix(dat, 2, byrow = FALSE)

x <- mat[1,]
y <- mat[2,]

plot(x,y)

print(paste("Covariance =", cov(x,y)))
print(paste("Correlation =", cor(x,y)))

c <- cor(x,y)
B <- 1000
D<- rep(c(0), B)

for (walk in 1:B){
  D[walk] = cor(x, sample(y, size=length(y), replace=FALSE))
}

s <- 0
for (i in D){
  if (i >= c){
    s <- s + 1
  }
}
print(s/B)