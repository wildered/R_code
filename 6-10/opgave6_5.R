dat <- scan('birdhops.txt')
x <- matrix(dat, ncol = 2, byrow = TRUE )
k <- length(x[1,])
m <- length(x[,1])

arr <- c()
for (row in 1:m){
  arr <- c(arr, rep(c(x[row, 1]), x[row,2]))
}

n <- length(arr)

hist(arr, probability = TRUE)

mu_1 <- mean(arr)
p_moment <- 1/mu_1
print(paste("Momentschatter p =", p_moment))

p_mle <- (n+1)/(n+1+sum(arr))

print(paste("Mle-schatter p =", p_mle))

v <- var(arr)

print(paste("Confidence interval = [", p_moment-1.96*sqrt(v)/sqrt(n), "," ,p_moment+1.96*sqrt(v)/sqrt(n),"]" , sep = ""))

asi <- seq(1,30,1)
lines(asi, dgeom(asi, p_moment), col='red')
lines(asi, dgeom(asi, p_mle), col='blue')

