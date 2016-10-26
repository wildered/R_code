n <- 20
as <- seq(-5, 5, length.out=1000)

c <- qnorm(0.95)/sqrt(n)

plot(as, 1-pnorm(c, as, sqrt(1/20)), type='l', col='red')


