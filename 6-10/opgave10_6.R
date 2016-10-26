x <- scan('Poisson.txt')
n <- length(x)

counts <- table(x)
barplot(counts)

print(paste("p-waarde =", 1-ppois(sum(x), lambda = n)))
