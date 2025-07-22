
enumerate <- 0L


# recursive vector ====
x <- lapply(1:20, \(x)sample(letters))
expect_equal(
  ii2_x(x, 2, red = TRUE),
  x[[2L]]
)
expect_equal(
  ii2_x(x, red = TRUE),
  x
)
enumerate <- enumerate + 2L


# recursive matrix ====
x <- matrix(lapply(1:20, \(x)sample(letters)), ncol = 4)
expect_equal(
  ss2_x(x, n(2), red = TRUE),
  x[[2,2]]
)
x <- matrix(lapply(1:20, \(x)sample(letters)), ncol = 1)
expect_equal(
  ss2_x(x, n(2L), 1L, red = TRUE),
  x[[2,1]]
)
x <- matrix(lapply(1:20, \(x)sample(letters)), nrow = 1)
expect_equal(
  ss2_x(x, n(2L), 2L, red = TRUE),
  x[[1,2]]
)
expect_equal(
  ss2_x(x, red = TRUE),
  x
)
enumerate <- enumerate + 3L


# recursive array ====
x <- array(lapply(1:27, \(x)sample(letters)), c(3,3,3))
expect_equal(
  ss2_x(x, n(2), red = TRUE),
  x[[2,2,2]]
)
expect_equal(
  ss2_x(x, red = TRUE),
  x
)

x <- array(lapply(1:20, \(x)sample(letters)), c(4,1,5))
expect_equal(
  ss2_x(x, n(2), c(1,3), red = TRUE),
  x[[2,1,2]]
)
expect_equal(
  ss2_x(x, red = TRUE),
  x
)
enumerate <- enumerate + 2L


