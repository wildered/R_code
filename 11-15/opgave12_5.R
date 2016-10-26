dat <- read.table("deathsus.txt")

x <- dat$V2
z <- dat$V1

barplot(x)
 
n <- length(x)

S122 = var(x)*(n-1)/n
X12 = mean(x)

print(paste("T =", S122/X12))
print(paste("Benaderde p-waarde:", pchisq(S122/X12, df = 11)))

B <- 10000
res <- numeric(B)
temp <- numeric(12)
l <- mean(x)

for (i in 1:B){
  temp = rpois(12, lambda = l)
  S122t = var(temp)*(n-1)/n
  X12t = mean(temp)
  T_temp = S122t/X12t 
  res[i] = (T_temp < T)
}
print(paste("Bootstrap p-waarde;", mean(res)))


