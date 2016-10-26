library(MASS)

x <- scan('whales.txt')
n <- length(x)

hist(x, probability = TRUE,breaks = 20)
as <- seq(0, 5, length=10000)

# lines(as, dgamma(as,0.5, 0.5), col='red')
# lines(as, dgamma(as,0.5, 1), col='blue')

#momentschatting
#\alpha = \lambda \mu_1

mu_1 = 0
mu_2 = 0
for (i in 1:n){
  mu_1 <- mu_1 + x[i]
  mu_2 <- mu_2 + x[i]^2
}
mu_1 <- mu_1/n
mu_2 <- mu_2/n

alpha <- (mu_1^2)/(mu_2-mu_1^2)
lambda <- (mu_1)/(mu_2-mu_1^2)

print(paste("alpha =", alpha, "lambda =", lambda))

lines(as, dgamma(as,alpha, lambda), col='red')

a <- fitdistr(x, 'gamma')
a_mle <- a$estimate[1]
l_mle <- a$estimate[2]

lines(as, dgamma(as,a_mle, l_mle), col='blue')
# print(a)

boot <- function(alpha_t, lambda_t, n, B){
  res_a <- c(1:B)
  res_l <- c(1:B)
  for (idx in 1:B){
    disti <- rgamma(n, alpha_t, lambda_t)
    tmu_1 <- mean(disti)
    tmu_2 <- var(disti) + tmu_1^2
    alpha_guess <- (tmu_1^2)/(tmu_2-tmu_1^2)
    lambda_guess <- (tmu_1)/(tmu_2-tmu_1^2)
    res_a[idx] <- alpha_guess
    res_l[idx] <- lambda_guess
    
  }
  return(list("alpha_r"= res_a, "lambda_r" = res_l))
}

boot_mle <- function(alpha_t, lambda_t, n, B){
  res_a <- c(1:B)
  res_l <- c(1:B)
  for (idx in 1:B){
    disti2 <- rgamma(n, alpha_t, lambda_t)
    at <- fitdistr(disti2, 'gamma')
    a_mle2 <- a$estimate[1]
    l_mle2 <- a$estimate[2]
    res_a[idx] <- a_mle2
    res_l[idx] <- l_mle2
    res_a[idx] <- alpha_guess
    res_l[idx] <- lambda_guess
    
  }
  return(list("alpha_r"= res_a, "lambda_r" = res_l))
}

temp <- boot(alpha, lambda, n, 1000)
alpha_array <- temp$alpha_r
lambda_array <- temp$lambda_r
cat('\n')
print(paste("Alpha Bootstrap Results Momentmethod"))
v1 <- var(alpha_array)
print(paste("Variance =", var(alpha_array)))
print(paste("Confidence interval = [", alpha-1.96*sqrt(v1), "," ,alpha+1.96*sqrt(v1),"]" , sep = ""))

cat("\n" )
print(paste("Lambda Bootstrap Results Momentmethod"))
v2 <- var(lambda_array)
print(paste("Variance =", var(lambda_array)))
print(paste("Confidence interval = [", lambda-1.96*sqrt(v2), "," ,lambda+1.96*sqrt(v2),"]" , sep = ""))

cat("\n" )

temp2 <- boot(a_mle, l_mle, n, 1000)
alpha_array2 <- temp2$alpha_r
lambda_array2 <- temp2$lambda_r

print(paste("Alpha Bootstrap Results "))
v3 <- var(alpha_array2)
print(paste("Variance =", var(alpha_array2)))
print(paste("Confidence interval = [", a_mle-1.96*sqrt(v3), "," ,a_mle+1.96*sqrt(v3),"]" , sep = ""))

cat("\n" )
print(paste("Lambda Bootstrap Results "))
v4 <- var(lambda_array2)
print(paste("Variance =", var(lambda_array2)))
print(paste("Confidence interval = [", l_mle-1.96*sqrt(v4), "," ,l_mle+1.96*sqrt(v4),"]" , sep = ""))




