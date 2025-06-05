
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
  ss_x(x, n(1:2)),
  x2
)
expect_equal(
  ss_wo(x, n(3:5, 3:4)),
  x2
)


# atomic 3d array ====
x.dim <- c(4,4,4)
x <- array(1:prod(x.dim), dim = x.dim)
nms <- c("a", "b", 'c', "d")
dimnames(x) <- list(nms, nms, nms)
x.names <- array(sample(letters, length(x), TRUE), c(4,4,4))
names(x) <- x.names
x2 <- x[1:2, 1:2, 1:2]
names(x2) <- x.names[1:2, 1:2, 1:2]

expect_equal(
  ss_x(x, list(1:2, 1:2, 1:2), 1:3),
  x2
)
expect_equal(
  ss_wo(x, list(3:4, 3:4, 3:4), 1:3),
  x2
)


# atomic 7d array ====
x.dim <- rep(4, 7)
x <- array(1:prod(x.dim), dim = x.dim)
nms <- c("a", "b", 'c', "d")
dimnames(x) <- rep(list(nms), 7)
x.names <- array(sample(letters, length(x), TRUE), rep(4, 7))
names(x) <- x.names
x2 <- x[1:2, 1:2, 1:2, 1:2, 1:2, 1:2, 1:2]
names(x2) <- x.names[1:2, 1:2, 1:2, 1:2, 1:2, 1:2, 1:2]

expect_equal(
  ss_x(x, rep(list(1:2), 7), 1:7),
  x2
)
expect_equal(
  ss_wo(x, rep(list(3:4), 7), 1:7),
  x2
)


# recursive matrix ====
x <- matrix(lapply(1:20, \(x)list(sample(letters), rnorm(10))), ncol = 4)
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- letters[1:5]
x.names <- letters[1:20]
names(x) <- x.names
dim(x.names) <- dim(x)
x2 <- x[1:2, 1:2]
names(x2) <- x.names[1:2, 1:2]

expect_equal(
  ss2_x(x, n(1:2)),
  x2
)
expect_equal(
  ss2_wo(x, n(3:5, 3:4)),
  x2
)

enumerate <- 8


# recursive array ====
x <- array(lapply(1:64, \(x)list(sample(letters), rnorm(10))), dim = c(4,4,4))
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- letters[1:4]
x.names <- sample(letters, 64, replace = TRUE)
names(x) <- x.names
dim(x.names) <- dim(x)
x2 <- x[1:2, 1:2,]
names(x2) <- x.names[1:2, 1:2,]

expect_equal(
  ss2_x(x, n(1:2, 1:2), 1:2),
  x2
)
expect_equal(
  ss2_wo(x, n(3:4, 3:4), 1:2),
  x2
)

enumerate <- 8

