
enumerate <- 0

# generic method not mutable errors ====
x <- matrix(1:20, 5, 4)
pattern <- "not a 'mutatomic' object"
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  ss_set(x, 1, rp = -1),
  pattern = pattern,
  fixed = TRUE
)
expect_error(
  sbt_set(x, 1, rp = -1),
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


x <- data.frame(a = letters[1:10], b = 1:10)
expect_error(
  sbt_set(x, col = "a", rp = letters[11:20])
)

x <- list( a = letters, b = 1:20)
expect_error(
  ii_set(x, i = "a", rp = letters[11:20])
)

enumerate <- enumerate + 10


# not a variable errors ====
expect_error(
  ii_set(as.mutatomic(1:10), i = 1, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)
expect_error(
  ss_set(as.mutatomic(array(1:10)), 1, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)
expect_error(
  sbt_set(as.mutatomic(matrix(1:10)), 1, rp = -1),
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
expect_error(
  sbt_set(data.table::data.table(a = letters, b = 1:26), col = 2, rp = -1),
  pattern = "only objects that exist as variables can be modified by reference"
)

enumerate <- enumerate + 6L


# cannot change value of locked binding for  errors ====
x <- mutatomic(1:20, dim = c(5,4), dimnames = n(letters[1:5], letters[1:4]))
lockBinding("x", environment())
expect_error(
  ii_set(x, i = 1, rp = -1),
  pattern = "cannot change value of locked binding for "
)
expect_error(
  ss_set(x, 1, rp = -1),
  pattern = "cannot change value of locked binding for "
)
expect_error(
  sbt_set(x, 1, rp = -1),
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

x <- data.table::data.table(a = letters, b = 1:26)
lockBinding("x", environment())
expect_error(
  sbt_set(x, 1, rp = -1),
  pattern = "cannot change value of locked binding for "
)
rm(list = "x")
enumerate <- enumerate + 6L


# set ====
# for atomic type checks: see test-generic_set_atomictypes.R

x <- data.table::data.table(a = 1:10, b = letters[1:10])
x2 <- x
sbt_set(x, col = "b", rp = list(letters[11:20]))
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 1


