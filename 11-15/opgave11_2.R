as <- seq(0, 1, length=10000)

plot(qunif(as), as^(1/2), type='l')

#b
# plot(qexp(as, 1), quantile(runif(n)^(1/3), as), type='l')
plot(qexp(as, 1), quantile(runif(10000)^(1/3), as), col='blue', type='l')
lines(qexp(as, 1),as^(1/3), col='red')
#c

n <- 1000
as <- seq(0, 1, length = n)
plot(qunif(as), sort(runif(n))^(1/2), type='l')

n <- 1000
as <- seq(0, 1, length = n)
plot(qexp(as), sort(runif(n))^(1/3), type='l')


