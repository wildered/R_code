N = 900
x <- scan('2_9.txt')
y <- mean(x)

print(paste('Mean = ', y))
z <- var(x)
n = length(x)

res <- 0
for (el in 1:n){
  res <- res + (x[el] - y)^2
}

S_n2 <- res/(n-1)
print(paste("Zuivere Schatter = ", S_n2))

S_n <- sqrt(S_n2)
print(paste("Mean confidence interval = [" , y - 1.96*S_n/(sqrt(n)) , "," , y + 1.96*S_n/(sqrt(n)), ']', sep=""))

uS = 900*y
print(paste("Total pop confidence interval = [" , uS - 1.96*S_n/(sqrt(n)) , "," , uS + 1.96*S_n/(sqrt(n)), ']', sep = ""))

