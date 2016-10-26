p <- seq(0, 1, length = 1000)

k <- c(0:10)
n <- 10

w2 <- (1/2)^n/((k/n)^k*((1-k/n)^(n-k)))

plot(k, w2, type='l', col='blue')
lines(1/2, 1, col='red', type='p')

n <- 10
c <- 2
print(paste("opgave d:", pbinom(n/2-c-1, size = n, p=1/2) + 1-pbinom(n/2+c, size=n, p=1/2)))
c <- 10
n <- 100
print(paste("opgave e exact:", pbinom(n/2-c-1, size = n, p=1/2) + 1-pbinom(n/2+c, size=n, p=1/2)))

print(paste("opgave d normaal benaderd:", 2*pnorm(-2, 0, 1)))
print(paste("opgave d normaal benaderd poging 2:", 2*pnorm(40, 50, 5)))


