x <- c(8,4,2,7,5,4,1,4,7,4)
n <- length(x)
k <- mean(x)

func <- function(lambda, n, B){
  res = c(1:B)
  for (idx in 1:B){
    mu <- mean(rpois(n, lambda))
    term <- sqrt(n)*(mu-lambda)/sqrt(mu)
    res[idx] <- term
  }
  return(res)
}

func2 <- function(lambda, n, B){
  res = c(1:B)
  for (idx in 1:B){
    mu <- mean(rpois(n, lambda))
    term <- sqrt(n)*(2*sqrt(mu)-2*sqrt(lambda))
    res[idx] <- term
  }
  return(res)
}

reals = func(k, n, 1000)

# hist(reals)

mu2 <- mean(reals)
al <- quantile(reals, 0.025)
br <- quantile(reals, 0.975)
print(paste("2,5% kwantiel =", al, "95.7% kwantiel =", br))

# print(paste("Bootstrap variance =", var(reals)))

sig2 <- var(reals)

left_term <- k-sqrt(k)/sqrt(n)*br
right_term <- k-sqrt(k)/sqrt(n)*al

# print(paste("Lambda 95% boostrap supported interval = [" , k - 1.96*sqrt(sig2) , "," , k+ 1.96*sqrt(sig2), ']', sep=""))
print(paste("Lambda 95% boostrap supported interval = [" , left_term , "," , right_term, ']' , sep=""))

cat("\n")


print("Tweede techniek")

reals2 = func2(k, n, 1000)

hist(reals2)
mu22 <- mean(reals2)
al2 <- quantile(reals2, 0.025)
br2 <- quantile(reals2, 0.975)
print(paste("2,5% kwantiel =", al2, "95.7% kwantiel =", br2))

# print(paste("Bootstrap variance =", var(reals)))

sig22 <- var(reals2)

left_term2 <- (sqrt(k)-br2/(2*sqrt(n)))^2
right_term2 <- (sqrt(k)-al2/(2*sqrt(n)))^2

# print(paste("Lambda 95% boostrap supported interval = [" , k - 1.96*sqrt(sig2) , "," , k+ 1.96*sqrt(sig2), ']', sep=""))
print(paste("Lambda 95% boostrap supported interval = [" , left_term2 , "," , right_term2, ']', sep=""))
