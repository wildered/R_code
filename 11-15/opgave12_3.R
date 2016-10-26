dat <- read.table('bearings.txt')
x <- dat$V1
y <- dat$V2
n <- length(x)

as <- seq(0, 20, length.out=1000)
plot(density(x))
plot(as, ecdf(x)(as), type='l')
boxplot(x)

a <- t.test(x,y)
print(a)

#p-waarde benaderen
B <- 10000
res <- numeric(B)
base <- mean(x)-mean(y)

s <- sum(c(x,y))
for (i in 1:B){
  z <- sample(c(x,y), size = n)
  res[i] <- ((abs(mean(z)-(s-sum(z))/n))>abs(base))
}

print(paste('p-waarde be adering', mean(res)))


