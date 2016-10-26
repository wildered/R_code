x <- scan('icea.txt')
y <- scan('iceb.txt')
xn <- mean(x)
yn <- mean(y)
base <- xn-yn

z <- c(x,y)

B <- 1000
res <- numeric(B)
est <- numeric(B)
total <- sum(z)
n <- length(z)
n1 <- length(x)
n2 <- length(y)

for (i in 1:B){
  temp <- sample(z, size = n1)
  res[i] = sum(temp)/n1-(total-sum(temp))/n2
  est[i] = (((sum(temp)/n1-(total-sum(temp))/n2) >base ) || ((-sum(temp)/n1-(total-sum(temp))/n2) > - base ))
  
}

hist(res, probability = TRUE)
lines(xn-yn, 1,col='red', type='p', lwd=10)

print(paste("Estimated p-value =", mean(est)))

