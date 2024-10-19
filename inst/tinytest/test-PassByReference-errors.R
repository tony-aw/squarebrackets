
enumerate <- 0

# generic method not mutable errors ====
x <- setNames(1:10, letters[1:10])
pattern <- "`x` is not a (supported) mutable object"
expect_error(
  sb_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)
x <- matrix(1:20, ncol = 4)
colnames(x) <- letters[1:4]
expect_error(
  sb_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)

x <- array(1:21, dim = c(3,3,3), dimnames = n(letters[1:3], letters[1:3], letters[1:3]))
expect_error(
  sb_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)

x <- data.frame(a = letters[1:10], b = 1:10)
expect_error(
  sb2_set(x, col = "a", rp = letters[11:20])
)

x <- list( a = letters, b = 1:20)
expect_error(
  sb2_set(x, i = "a", rp = letters[11:20])
)

enumerate <- enumerate + 10


# not a variable errors ====

expect_error(
  sb_set(as.mutable_atomic(1:10), i = 1, rp = -1),
  pattern = "only existing variables can be modified by reference"
)

expect_error(
  setapply(mutable_atomic(1:10, dim = c(2, 5)), 1, sum),
  pattern = "only existing variables can be modified by reference"
)
expect_error(
  ma_setv(as.mutable_atomic(1:10), 1, 10),
  pattern = "only existing variables can be modified by reference"
)
enumerate <- enumerate + 4


# object is locked errors ====
x <- mutable_atomic(1:20, dim = c(5,4), dimnames = n(letters[1:5], letters[1:4]))
lockBinding("x", environment())
expect_error(
  sb_set(x, i = 1, rp = -1),
  pattern = "object is locked"
)
expect_error(
  setapply(x, 1, sum),
  pattern = "object is locked"
)
expect_error(
  ma_setv(x, 1, 1),
  pattern = "object is locked"
)

rm(list = "x")
enumerate <- enumerate + 5


# must be a data.table errors ====

x <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
expect_error(
  dt_setcoe(x, vars = is.numeric, v = as.numeric),
  pattern = "`x` must be a data.table"
)
expect_error(
  dt_setrm(x, col = 1),
  pattern = "`x` must be a data.table"
)
new <- data.table::data.table(e = factor(letters[1:10]))
expect_error(
  dt_setadd(x, new),
  pattern = "`x` must be a data.table"
)

enumerate <- enumerate + 3


# sb_set() ====
# for atomic type checks: see test-generic_set_atomictypes.R

x <- data.table::data.table(a = 1:10, b = letters[1:10])
x2 <- x
sb2_set(x, col = "b", rp = list(letters[11:20]))
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 1


# setapply() ====
# see the script test-setapply.R


# ma_setv ====
# see the script test-ma_setv.R
