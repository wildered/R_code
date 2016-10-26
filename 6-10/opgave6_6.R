library(MASS)

x <- scan('randnum.txt')
y <- fitdistr(x, 'normal')

hist(x)
print(y)