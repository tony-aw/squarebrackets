
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0


# array ====
x <- array(rnorm(100), c(5:3))
expect_equal(
  idx(x, slice = 1:2 * -1i, margin = 1),
  5:4
)
expect_equal(
  idx(x, slice = 1:2 * -1i, margin = 2),
  4:3
)
expect_equal(
  idx(x, slice = 1:2 * -1i, margin = 3),
  3:2
)
enumerate <- enumerate + 3L


# data.frame ====
x <- data.frame(a = 1:12, b = month.abb, c = 1:12 * -1i)
expect_equal(
  idx(x, 1:3 * -1i, 1),
  12:10
)
expect_equal(
  idx(x, 1:2 * -1i, 2),
  3:2
)
enumerate <- enumerate + 2L



