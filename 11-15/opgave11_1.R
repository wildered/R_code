as <- seq(0, 5, length=10000)
plot(as, pexp(as, 1/2), type='l', col='blue')

n <- 10
x <- rexp(n, 1/2)
emp <- ecdf(x)
lines(as, emp(as), col='red')


n <- 1000
x <- rexp(n, 1/2)
emp <- ecdf(x)
lines(as, emp(as), col='green')


n <- 10000
x <- rexp(n, 1/2)
emp <- ecdf(x)
lines(as, emp(as), col = 'yellow')

#beta distribution
as <- seq(0, 1, length=10000)
plot(as, pbeta(as, 2, 5), type='l', col='blue')

n <- 10
x <- rbeta(n, 2, 5)
emp <- ecdf(x)
lines(as, emp(as), col='red')

n <- 1000
x <- rbeta(n, 2, 5)
emp <- ecdf(x)
lines(as, emp(as), col='green')

n <- 10000
x <- rbeta(n, 2, 5)
emp <- ecdf(x)
lines(as, emp(as), col='yellow')

#normal
as <- seq(-3, 5, length=10000)
plot(as, pnorm(as, 1, 1), type='l', col='blue')


n <- 10
x <- rnorm(n, 1, 1)
emp <- ecdf(x)
lines(as, emp(as), col='red')

n <- 1000
x <- rnorm(n, 1, 1)
emp <- ecdf(x)
lines(as, emp(as), col='green')

n <- 10000
x <- rnorm(n, 1, 1)
emp <- ecdf(x)
lines(as, emp(as), col='yellow')

