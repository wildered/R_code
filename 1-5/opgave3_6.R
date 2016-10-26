x <- scan('gamma-arrivals.txt')

#a
hist(x, probability=TRUE)

n <- length(x)

mu_schat1 <- 0
mu_schat2 <- 0
for (walk in 1:n){
  mu_schat1 <- mu_schat1 + x[walk]
  mu_schat2 <- mu_schat2 + x[walk]^2
}
mu_schat1 <- mu_schat1/n
mu_schat2 <- mu_schat2/n


print(paste("^mu'[^]_1 =", mu_schat1))

print(paste("mu_2 =", mu_schat2))

alpha_schat <- mu_schat1^2/(mu_schat2 - mu_schat1^2)

lambda_schat = mu_schat1/(mu_schat2 - mu_schat1^2)

cat('\n')

print(paste("^alpha =", alpha_schat))
print(paste("^lamdba = ", lambda_schat))

as <- seq(-3, 1000, length=500)

lines(as, dgamma(as,alpha_schat, lambda_schat), col='red')
