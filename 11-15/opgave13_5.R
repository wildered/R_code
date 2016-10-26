x <- scan('ozonecontrol.txt')
y <- scan('ozonetreat.txt')

print("Gegevens ongepaard")

as <- -20:40
as <- seq(-20, 40, by = 1)
hist(x, breaks=20, probability = TRUE)
lines(as, dnorm(as, mean(x), sqrt(var(x))))

as <- -20:50
as <- seq(-20, 50, by = 1)
hist(y, breaks=20, probability = TRUE)
lines(as, dnorm(as, mean(y), sqrt(var(y))))

