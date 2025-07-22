
enumerate <- 0

# generic method not mutable errors ====
x <- setNames(1:10, letters[1:10])
pattern <- "not a 'mutatomic' object"
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  slice_set(x, 1, 10, rp = -1),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  slicev_set(x, v = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)

x <- matrix(1:20, ncol = 4)
colnames(x) <- letters[1:4]
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)

x <- array(1:21, dim = c(3,3,3), dimnames = n(letters[1:3], letters[1:3], letters[1:3]))
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)

x <- data.frame(a = letters[1:10], b = 1:10)
expect_error(
  ss2_set(x, col = "a", rp = letters[11:20])
)

x <- list( a = letters, b = 1:20)
expect_error(
  ii2_set(x, i = "a", rp = letters[11:20])
)

enumerate <- enumerate + 10


# not a variable errors ====

expect_error(
  ii_set(as.mutatomic(1:10), i = 1, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)
expect_error(
  slice_set(as.mutatomic(1:10), 1, 10, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)
expect_error(
  slicev_set(as.mutatomic(1:10), v = 1, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)

enumerate <- enumerate + 4


# cannot change value of locked binding for  errors ====
x <- mutatomic(1:20, dim = c(5,4), dimnames = n(letters[1:5], letters[1:4]))
lockBinding("x", environment())
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = "cannot change value of locked binding for "
)
expect_error(
  slice_set(x, 1, 10, rp = -1),
  pattern = "cannot change value of locked binding for "
)
expect_error(
  slicev_set(x, v = 1, rp = -1),
  pattern = "cannot change value of locked binding for "
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
  dt_setrm(x, vars = 1),
  pattern = "`x` must be a data.table"
)
new <- data.table::data.table(e = factor(letters[1:10]))
expect_error(
  dt_setadd(x, new),
  pattern = "`x` must be a data.table"
)

enumerate <- enumerate + 3


# set ====
# for atomic type checks: see test-generic_set_atomictypes.R

x <- data.table::data.table(a = 1:10, b = letters[1:10])
x2 <- x
ss2_set(x, vars = "b", rp = list(letters[11:20]))
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 1


