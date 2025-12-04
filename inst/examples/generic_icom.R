
# atomic ====

x <- 1:10
x[ii_icom(x, \(x)x>5)] <- -5
print(x)

x <- array(1:27, dim = c(3,3,3))
x[ss_icom(x, n(1:2, 1:2), c(1,3))] <- -10
print(x)


################################################################################


# recursive ====

x <- as.list(1:10)
x[ii_icom(x, \(x)x>5)] <- -5
print(x)

x <- array(as.list(1:27), dim = c(3,3,3))
x[ss_icom(x, n(1:2, 1:2), c(1,3))] <- -10
print(x)


x <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE),
  b = 1:10,
  c = rnorm(10),
  d = letters[1:10],
  e = factor(letters[11:20])
)
rows <- sbt_icom(x, 1:5, -1)
cols <- sbt_icom(x, c("b", "a"), 2)
x[rows, cols] <- NA
print(x)

