x <- scan('RL.txt')
n <- length(x)

hist(x, probability = TRUE)
F <- ecdf(x)

as <- seq(0, 5, length.out=1000)
plot(as, F(as), type='l', col='red')

lines(as, 1-exp(-as^2/2), col='blue')
p <- seq(0, 1, length.out=1000)

finv <- function(y){
  return(sqrt(-2*log(1-y)))
}

plot(finv(p),quantile(x, p), type='l', col='blue')
lines(c(0, 10), c(0, 10), col='red')

n <- length(x)
x<- sort(x)
plot(x, x, type='n')
lines(c(0,5), c(0,5), col='red')
for (i in 1:(n-1)){
  lines(c(finv(i/(n+1)), finv((i+1)/(n+1))), c(x[i], x[i+1]), col='blue')
}

n <- length(x)
x<- sort(x)
plot(c(1:n)/(n+1), 1-exp(-x^2/2), type='n')
lines(c(0,1), c(0,1), col='red')
for (i in 1:(n-1)){
  lines(c((i/(n+1)), (i+1)/(n+1)), c(1-exp(-x[i]^2/2), 1-exp(-x[i+1]^2/2)), col='blue')
}

hist(x, probability = TRUE)
lines(density(x, adjust = 1), col='blue')
lines(as, as*exp(-as^2/2), type='l', col='red')
n <- length(x)
s <- sum(x^2)
sigma <- sqrt(s/n)
lines(as, as/sigma*exp(-as^2/sigma^2), col='green')


rl <- function(x){
  return(1-exp(-x^2/2))
}

rl2 <- function(x){
  return(1-exp(-x^2*(sigma-0.052)/2))
}

print(ks.test(x, rl))
print(ks.test(x, rl2))

