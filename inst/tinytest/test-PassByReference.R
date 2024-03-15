
enumerate <- 0

# sb_set() ====

x <- as.mutable_atomic(1:10)
x2 <- x
sb_set(x, i = 1, rp = -1)
expect_equal(
  x,
  x2
)

x <- as.mutable_atomic(matrix(1:20, ncol = 4))
x2 <- x
sb_set(x, row = 2:3, col = 2:3, rp = -1)
expect_equal(
  x,
  x2
)

x <- as.mutable_atomic(matrix(sample(1:100), ncol = 10))
x2 <- x
setapply(x, 1, sort)
expect_equal(
  x,
  x2
)

x <- as.mutable_atomic(matrix(sample(1:100), ncol = 10))
x2 <- x
setapply(x, 2, sort)
expect_equal(
  x,
  x2
)

x <- as.mutable_atomic(array(1:27, dim = c(3,3,3)))
x2 <- x
sb_set(x, rcl = n(2:3, NULL, 2:3), rp = -1)
expect_equal(
  x,
  x2
)

x <- as.mutable_atomic(array(1:81, dim = c(3,3,3,3)))
x2 <- x
sb_set(x, idx = n(2:3, 2:3), dims = c(1,4), rp = -1)
expect_equal(
  x,
  x2
)

x <- data.table::data.table(a = 1:10, b = letters[1:10])
x2 <- x
sb_set(x, col = "b", rp = list(letters[11:20]))
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 5


# setapply() ====

x <- mutable_atomic(sample(1:90), dim = c(9,10))
x2 <- x
setapply(x, 1, sort)
expect_equal(
  x,
  x2
)

x <- mutable_atomic(sample(1:90), dim = c(9,10))
x2 <- x
setapply(x, 2, sort)
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 2

# sb_setRename() ====
# see the script test-generic_setRename

