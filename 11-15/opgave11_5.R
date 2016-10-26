x <- scan('meas.txt')

F <- ecdf(x)

as <- seq(30, 70, length=10000)
plot(as, F(as), type='l')
mu <- mean(x)
s <- sqrt((n-1)/n*var(x))
lines(as, pnorm(q = as, mu, s), col='blue')



hist(x, probability = TRUE, breaks = 18)
d <- density(x, adjust=1)
lines(as, dnorm(as, mu, s), col='red')

lines(d, col='red')


