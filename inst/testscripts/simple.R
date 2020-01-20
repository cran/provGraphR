a <- 1:10
b <- 2:11
df <- data.frame (a, b)
a <- a + 1
lines <- readLines ("simple.R")

a <- a + a
a <- b <- a + b
a <- a <- a + b
