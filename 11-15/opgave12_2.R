datt <- scan('wax.txt')

dat <- matrix(datt, ncol = 2, byrow=TRUE)

x <- dat[,1]
y <- dat[,2]

as <- seq(0, 1, length.out = 1000)

# plot(quantile(x, as), quantile(y, as), col='blue', type='l')
plot(quantile(x, as), qnorm(as), col='blue', type='l')

as <- seq(62, 65, length.out=1000)
f <- ecdf(x)
plot(as, f(as), type='l', col='blue')

n <- length(x)
mu <- mean(x)
sig2 <- var(x)
lines(as, pnorm(as, mu, sd = sqrt(sig2)), col='red' )

k <- function(t){
  return(pnorm(t, mu, sqrt(sig2)))
}

print(ks.test(unique(x),k))

B <- 10000
res <- numeric(B)
base_v <- ks.test(unique(x), pnorm, mu, sqrt(sig2))[1]$statistic

for (i in 1:B){
  a <- rnorm(n, mu, sqrt(sig2))
  res[i] = (ks.test(unique(a), k)[1]$statistic>base_v)
}

print(paste('p-value =',mean(res)))

