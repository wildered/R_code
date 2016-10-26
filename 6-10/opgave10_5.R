x <- scan('mustijgend.txt')
n <- length(x)
k <- 5
print("k = 5")

as = c(1:n)
plot(as, x)

T <- function(x, n,k){
  return(sum(x[(n-k+1):(n)])-sum(x[0:k]))
}

print(paste('k = 5 alpha 0.05 geeft:', qnorm(0.95, 0, 2*5)))
print(paste('k = 15 alpha 0.05 geeft:', qnorm(0.95, 0, 2*15)))
if (T(x,n, k) < qnorm(0.95, 0, 2*k)){
  print("Accept H_0")
} else {
  print("Reject H_0")
}


k <- 15
print(T(x,n,k))
print(paste("k =", k))

if (T(x,n, k) < qnorm(0.95, 0, 2*k)){
  print("Accept H_0")
} else {
  print("Reject H_0")
}


cat('\n')
print('Permutation Test')

k <- 5
print(paste('k =', k))

count <- 0
l <- 1000
baseline <- T(x,n, k)
total <- l
for (walk in 1:l){
  xdat <- sample(x, n, replace = FALSE)
  if (T(xdat,n,k) > baseline){
    count <- count + 1
  }
}
print(paste('k = 5 PermTest gives p-value=', count/total))

k <- 15
print(paste('k =', k))

count <- 0
l <- 1000
baseline <- T(x,n, k)
total <- l
for (walk in 1:l){
  xdat <- sample(x, n, replace = FALSE)
  if (T(xdat,n,k) > baseline){
    count <- count + 1
  }
}
print(paste('k = 15 PermTest gives p-value=', count/total))

