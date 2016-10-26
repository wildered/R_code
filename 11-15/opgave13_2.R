B <- 10000
res <- numeric(B)
base <- 16

for (i in 1:B){
  dat <- rnorm(25)
  s <- 0
  for (el in dat){
    if (el > 0){
      s <- s+el
    }
  }
  res[i] = (s>base)
}

print(paste("result =", mean(res)))

print(paste("Answer question 1:", 1-pbinom(q = 16, size = 25, prob = 1/2)))

c <- qnorm(0.946)/5

print(paste("LR grens =", c))

mu <- seq(-1, 1.5, length.out = 1000)

plot(mu, 1-pnorm(5*(c-mu)), type='l', col='red')

p <- 1-pnorm(0, mean = mu)
lines(mu, 1-pbinom(q = 16, size = 25,prob = p), col='blue')

print(paste("Significance", 1-pbinom(16, 25, 1-pcauchy(0))))

lines(mu, 1-pbinom(16, 25, 1-pcauchy(0, location = mu)))

c2 <- qcauchy(0.946)

lines(mu, 1-pcauchy(c2, location = mu), col='green')




