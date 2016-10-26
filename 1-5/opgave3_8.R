dat <- scan('3_8.txt')
m <- length(dat)/4

temp <- c(1:4*400)
y <- matrix(dat, ncol = 4, byrow = TRUE)

f <- c()
for (t in 1:4){

  for (index in 1:m){
    a <- c(index-1)
    f <- c(f, rep(a,rep(y[index, t])))
    
  }
}

x <- matrix(f, ncol = 400, byrow = TRUE)
x <- t(x)

n <- length(x[, 1])

for (stap in 1:4){
  print(paste('Momentschatting ', stap, ' = ', mean(x[,stap]), sep=""))
}


print("")
print("Asympotisch: ")
for (idx in 1:4){
  print(paste("Interval for ", idx,   ' gives = [' , mean(x[,idx]) - 1.96*sqrt(var(x[,idx]))/sqrt(n) , "," , mean(x[,idx]) + 1.96*sqrt(var(x[,idx]))/sqrt(n), ']', sep=""))
}

print("")
print("Bootstrap method")

func <- function(lambda, nk, B){
  res = c(1:B)
  for (idx in 1:B){
    mu <- mean(rpois(nk, lambda))
    term <- sqrt(nk)*(mu-lambda)/sqrt(mu)
    res[idx] <- mu
  }
  return(res)
}

for (walk2 in 1:4){
  mu = mean(x[,walk2])
    reals = func(mu, n, 1000)

  print(paste("Bootstrap variance =", var(reals)))
  
  sig2 <- var(reals)
  
  print(paste("Bootstrap given interval = [" , mu - 1.96*sqrt(sig2) , "," , mu + 1.96*sqrt(sig2), ']', sep=""))
  
}
