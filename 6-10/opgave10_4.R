print(paste("opgave d alleen 10:", 2*qchisq(0.95, df=10)))
x <- scan("kwadrnorm.txt")

n <- length(x)

c <- qchisq(0.95, df=n)

s <- sum(x^2)

if (s>c){
  print('H_0 verwerpen!')
} else{
  print('H_0 accepteren!')
}

print(paste('p-waarde =', 1-pchisq(s, df=n)))

