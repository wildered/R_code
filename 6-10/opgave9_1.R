x <- seq(0, 100, length = 101)
p <- seq(0, 1, length = 1000)
y <- dbinom(x, 100, prob = 1/2)


a <- 1/2-0.025
b <- 1/2+0.025
print(paste('Onderscheidend vermogen ', -2*(b^3-a^3) + 3*(b^2 - a^2)))

print(paste('significance alpha =', pbinom(39, 100, 1/2)+(1-pbinom(60, 100, 1/2))))

print(paste('normal approx significance alpha =', pnorm((39.5-50)/5,0, 1)+(1-pnorm((60.5 -50)/5,mean = 0, sd = 1))))

print(paste('Try 2: normal approx significance alpha =', 2*pnorm(-2,0, 1)))


z <- pbinom(39, size=100, prob=p) + (1-pbinom(60, size = 100, prob = p ))

zn <- pnorm((39.5-100*p)/(10*sqrt(p*(1-p))),0, 1) + (1-pnorm((60.5-100*p)/(10*sqrt(p*(1-p))), 0, 1))

plot(p, z, type = 'l')
lines(p, zn, col='red')

print( qnorm(p = 0.9, 0, sqrt(100/25)))
c <- qnorm(p = 0.9, 0, sqrt(100/25))

print(dnorm(c, 1.5, sqrt(100/25)))
print( qnorm(p = 0.99, 0, sqrt(100/25)))

c <- qnorm(p = 0.99, 0, sqrt(100/25))
print(dnorm(c, 1.5, sqrt(100/25)))
print(1-pnorm(7, 5, 1))

mu2 <- seq(5, 15, length=1000)
be_y <- 1-pnorm(7, mu2, 1)
plot(mu2, be_y, type='l')

mu23 <- seq(-5, 15, length=1000)
be_y3 <- 1-pnorm(7, mu23, 1) 

#lines(mu23, be_y3, col='red')
plot(mu23, be_y3, type='l', col='blue')

c <- 1-pnorm(7, 5, 1)
print(5-qnorm(c/2, 5,1 ))

k <- 5-qnorm(c/2, 5,1 )
y_3 <- pnorm(5-k, mu23, 1) + 1- pnorm(5+k, mu23, 1)

lines(mu23, y_3, col='red')
lines(5, c, type='p')

