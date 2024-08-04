

x <- array(1:120, dim = 4:6)


expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, sys.call()),
  ci_sub(x, n(1:5, 1:4, 1:3), 3:1, FALSE, FALSE, sys.call())
)
expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, sys.call()),
  ci_sub(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, FALSE, sys.call())
)
expect_error(
  ci_sub(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, FALSE, sys.call()),
  pattern = "`dims` out of range",
  fixed = TRUE
)


expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, sys.call()),
  ci_sub(x, n(1:5, 1:4, 1:3), 3:1, FALSE, inv = TRUE, sys.call())
)
expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, sys.call()),
  ci_sub(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, inv = TRUE, sys.call())
)
expect_error(
  ci_sub(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, inv = TRUE, sys.call()),
  pattern = "`dims` out of range",
  fixed = TRUE
)

enumerate <- 9
