
# atomic matrix ====

x <- matrix(1:20, ncol = 4)
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- letters[1:5]
x.names <- letters[1:20]
names(x) <- x.names
dim(x.names) <- dim(x)
x2 <- x[1:2, 1:2]
names(x2) <- x.names[1:2, 1:2]

expect_equal(
  sb_x(x, 1:2, 1:2),
  x2
)
expect_equal(
  sb_rm(x, 3:5, 3:4),
  x2
)


# atomic array ====

dims <- c(4,4,4)
x <- array(1:prod(dims), dim =dims)
nms <- c("a", "b", 'c', "d")
dimnames(x) <- list(nms, nms, nms)
x.names <- array(sample(letters, length(x), TRUE), c(4,4,4))
names(x) <- x.names
x2 <- x[1:2, 1:2, 1:2]
names(x2) <- x.names[1:2, 1:2, 1:2]

expect_equal(
  sb_x(x, list(1:2, 1:2, 1:2), 1:3),
  x2
)
expect_equal(
  sb_rm(x, list(3:4, 3:4, 3:4), 1:3),
  x2
)


# recursive array ====

x <- matrix(lapply(1:20, \(x)list(sample(letters), rnorm(10))), ncol = 4)
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- letters[1:5]
x.names <- letters[1:20]
names(x) <- x.names
dim(x.names) <- dim(x)
x2 <- x[1:2, 1:2]
names(x2) <- x.names[1:2, 1:2]

expect_equal(
  sb2_x(x, n(1:2, 1:2), 1:2, cn2 = FALSE),
  x2
)
expect_equal(
  sb2_rm(x, n(3:5, 3:4), 1:2, cn2 = FALSE),
  x2
)

enumerate <- 6

