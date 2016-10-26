dat <- read.table('ampicillin.txt', header = TRUE)

x <- dat$microbiological
y <- dat$hydroxylaminie
z <- x-y

n <- length(x)

print(cor(x,y))
print(mean(x))
print(mean(y))

print(t.test(x,y, paired = TRUE))

F <- ecdf(z)

plot(F)
mu = mean(z)
sig = sqrt(sum((z-mu)^2)/15)
as <- seq(-10, 10, length.out= 1000)
lines(as, pnorm(as, mu, sig), col='red')
print(ks.test(unique(z), pnorm, mu, sig))
kst <- ks.test(unique(z), pnorm, mu, sig)

B <- 10000
res <- numeric(B)
base <- kst$statistic

for (i in 1:B){
  steekproef = rnorm(n, mu, sig)
  temp <- ks.test(steekproef, pnorm, mu, sig)
  res[i] <- (temp$statistic > base)
}

print(paste("benadered p-waarde =", mean(res)))

xt <- c()
for (idx1 in 1:n){
  flag = TRUE
  for (idx2 in 1:n){
    if (x[idx1] == y[idx2]){
      flag = FALSE
      break
    }
  }
  if (flag){xt <- c(xt, x[idx1])}
}
yt <- c()
for (idx1 in 1:n){
  flag = TRUE
  for (idx2 in 1:n){
    if (y[idx1] == x[idx2]){
      flag = FALSE
      break
    }
  }
  if (flag){yt <- c(yt, y[idx1])}
}
xt <- unique(xt)
yt <- unique(yt)




print("opgave e)")
base <- abs(mean(x)-mean(y))
B <- 10000
res <- numeric(B)
for (i in 1:B){
  temp <- 0
  for (k in 1:n){
    temp <- temp + sample(c(z[k], -z[k]), size = 1)
  avg <- temp/n
  res[i] <- (abs(avg) > base)
  }
}

print(paste("Niet normaal benaderde p-waarde =", mean(res)))

print(wilcox.test(xt,yt))

xc <- unique(setdiff(x, y))
yc <- unique(setdiff(y,x))
print(wilcox.test(xc, yc))


