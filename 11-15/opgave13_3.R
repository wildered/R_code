n <- 20
a <- 0.05
c <- sqrt(200)/sqrt(n)*qnorm(1-a/2)

mu <- seq(-20, 20, length.out=1000)

# plot(mu, pnorm(-c, mean=mu, sd=sqrt(200/n)) + 1-pnorm(c, mean=mu, sd=sqrt(200/n)), type='l', col='red')
plot(mu, 1-pnorm(sqrt(n/200)*(c-mu))+pnorm(sqrt(n/200)*(-c-mu)), type='l', col='red')


n <- 20
a <- 0.10
c <- sqrt(200)/sqrt(n)*qnorm(1-a/2)

mu <- seq(-20, 20, length.out=1000)

# plot(mu, pnorm(-c, mean=mu, sd=sqrt(200/n)) + 1-pnorm(c, mean=mu, sd=sqrt(200/n)), type='l', col='red')
lines(mu, 1-pnorm(sqrt(n/200)*(c-mu))+pnorm(sqrt(n/200)*(-c-mu)), col='blue')

n <- 40
a <- 0.05
c <- sqrt(200)/sqrt(n)*qnorm(1-a/2)

mu <- seq(-20, 20, length.out=1000)

# plot(mu, pnorm(-c, mean=mu, sd=sqrt(200/n)) + 1-pnorm(c, mean=mu, sd=sqrt(200/n)), type='l', col='red')
lines(mu, 1-pnorm(sqrt(n/200)*(c-mu))+pnorm(sqrt(n/200)*(-c-mu)), col='black')

n <- 40
a <- 0.10
c <- sqrt(200)/sqrt(n)*qnorm(1-a/2)

mu <- seq(-20, 20, length.out=1000)

# plot(mu, pnorm(-c, mean=mu, sd=sqrt(200/n)) + 1-pnorm(c, mean=mu, sd=sqrt(200/n)), type='l', col='red')
lines(mu, 1-pnorm(sqrt(n/200)*(c-mu))+pnorm(sqrt(n/200)*(-c-mu)), col='green')



