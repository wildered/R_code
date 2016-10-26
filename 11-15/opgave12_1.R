x <- scan('icea.txt')
y <- scan('iceb.txt')

boxplot(x, y)

F1 <- ecdf(x)
F2 <- ecdf(y)

z <- seq(79.9, 80.2, length.out=1000)
plot(z,F2(z), type='l')
lines(z,F1(z), col='blue')
