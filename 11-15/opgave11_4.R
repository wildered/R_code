n <- 20
x <- rgamma(n, 3, 1)

hist(x, probability = TRUE)
count <- 10

for (v in c(1/2, 3/4, 1, 5/4, 6/4, 2)){
  d <- density(x, adjust=v)
  lines(d, col=count)
  count <- count + 7
}


n <- 2000
x <- rgamma(n, 3, 1)

hist(x, probability = TRUE)
count <- 10

for (v in c( 1/4, 1/2, 3/4, 1, 5/4, 6/4, 2)){
  d <- density(x, adjust=v)
  lines(d, col=count)
  count <- count + 7
}



