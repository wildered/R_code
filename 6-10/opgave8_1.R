print(paste("Kans op X>=55 =", 1-pbinom(54, size = 100, 0.5)))
print(paste("Kans op X>=65 =", 1-pbinom(64, size = 100, 0.5)))

k <- 0
a <- 0
b <- 0

while (a+b < 0.10){
  arr <- pbinom(c(k,20-k-1), size=20, prob = 1/2)
  a <- arr[1]
  b <- 1-arr[2]
  k <- k + 1
}
k <- k - 1
k <- k - 1
arr <- pbinom(c(k,20-k-1), size=20, prob = 1/2)
a <- arr[1]
b <- 1-arr[2]

print(paste('Probability to reject H_0 in error = ', a+ b))

x <- seq(0, 1, length=1000)
y <- seq(0,1,length=1000)
for (idx in 1:1000){
  temp <- pbinom(c(1,18), size=20, p = x[idx])
  a <- temp[1]
  b <- 1-temp[2]
  y[idx] <- a+b
}

plot(x,y, type = 'l')
