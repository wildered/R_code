dat <- read.table('calcium.txt')

x <- dat$V1
y <- dat$V2

diff <- x-y

print(t.test(x,y))


