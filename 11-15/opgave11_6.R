library("vioplot")

x <- scan('meas.txt')

print(paste('Mean =', mean(x)))
print(paste('Trimmed Mean =', mean(x, trim = 0.05)))

print(paste('Median =', median(x)))
print(paste("Standaard afwijking =", sd(x)))
print(paste("Interkwartiel afstand =", quantile(x, 0.75) - quantile(x, 0.25)))


cat('New Data points x_101 = 100 \n \n')

x2 <- c(x, 100)

print(paste('Mean =', mean(x2)))
print(paste('Trimmed Mean =', mean(x2, trim = 0.05)))

print(paste('Median =', median(x2)))
print(paste("Standaard afwijking =", sd(x2)))
print(paste("Interkwartiel afstand =", quantile(x2, 0.75) - quantile(x2, 0.25)))

cat('New Data points x_101 = 100 \n \n')

x2 <- c(x, 100000)

print(paste('Mean =', mean(x2)))
print(paste('T     Nrimmed Mean =', mean(x2, trim = 0.05)))

print(paste('Median =', median(x2)))
print(paste("Standaard afwijking =", sd(x2)))
print(paste("Interkwartiel afstand =", quantile(x2, 0.75) - quantile(x2, 0.25)))

boxplot(x)
vioplot(x)

