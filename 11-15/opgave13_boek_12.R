dat <- read.table('fishmercury.txt', sep = ',', header=TRUE)
x<- dat$reduction
y<- dat$permanganate
z <- y-x
z <- z[z!=0]
n <- 25

a = 0.05
low <- qbinom(a/2, n, 1/2)
high <- qbinom(1-a/2, n, 1/2)

poscount <- length(z[z>0])
print(poscount)

if (poscount < low || poscount > high){
  print("Reject H_0")
} else{
  print("Accept H_0")
}


