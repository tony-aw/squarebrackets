
# atomic ====

x <- 1:10
x[idx1(x, \(x)x>5)] <- -5
print(x)

x <- matrix(1:20, ncol = 4)
colnames(x) <- letters[1:4]
x[idx1(x, 1:2, c("a", "b"))] <- -5
print(x)

x <- array(1:27, dim = c(3,3,3))
x[idx1(x, n(1:2, 1:2), c(1,3))] <- -10
print(x)

x <- array(1:27, dim = c(3,3,3))
x[idx1(x, rcl = n(1:2, 1:2, NULL))] <- -10
print(x)


################################################################################


# recursive ====

x <- as.list(1:10)
x[idx1(x, \(x)x>5)] <- -5
print(x)

x <- matrix(as.list(1:20), ncol = 4)
colnames(x) <- letters[1:4]
x[idx1(x, 1:2, c("a", "b"))] <- -5
print(x)

x <- array(as.list(1:27), dim = c(3,3,3))
x[idx1(x, n(1:2, 1:2), c(1,3))] <- -10
print(x)


x <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE),
  b = 1:10,
  c = rnorm(10),
  d = letters[1:10],
  e = factor(letters[11:20])
)
rows <- idx1_dim(x, 1:5, 1, inv = TRUE)
cols <- idx1_dim(x, c("b", "a"), 2)
x[rows, cols] <- NA
print(x)
