print(paste("Opgave b:", qnorm(0.95, 0, 1)))

c <- qnorm(0.95, 0, 1)

mu1 <- seq(0, 3, length=1000)
y <- 1-pnorm(c, mu1, 1)

plot(mu1, y, type='l', col='blue')

print(paste("Opgave h:", pnorm(-1.96, 0, 1)+1-pnorm(1.96, 0, 1)))

z <- pnorm(-1.96, mu1, 1)+1-pnorm(1.96, mu1, 1)

lines(mu1, z, col='red')

