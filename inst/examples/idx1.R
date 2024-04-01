
# atomic ====

x <- 1:10
x[idx1(x, \(x)x>5)] <- -5
print(x)

x <- matrix(1:20, ncol = 4)
x[idx1(x, 1:2, 1:2)] <- -5
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
x[idx1(x, 1:2, 1:2)] <- -5
print(x)

x <- array(as.list(1:27), dim = c(3,3,3))
x[idx1(x, n(1:2, 1:2), c(1,3))] <- -10
print(x)

x <- array(as.list(1:27), dim = c(3,3,3))
x[idx1(x, rcl = n(1:2, 1:2, NULL))] <- -10
print(x)

