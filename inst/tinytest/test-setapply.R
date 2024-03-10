
x <- mutable_atomic(sample(1:90), dim = c(9,10))
x2 <- x
for(i in 1:nrow(x)) x2[i,] <- sort(x2[i,]) 
setapply(x, 1, sort)
expect_equal(
  x,
  as.mutable_atomic(x2)
)

x <- mutable_atomic(sample(1:90), dim = c(9,10))
x2 <- x
for(i in 1:ncol(x)) x2[,i] <- sort(x2[,i]) 
setapply(x, 2, sort)
expect_equal(
  x,
  as.mutable_atomic(x2)
)
