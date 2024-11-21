

enumerate <- 0L

x <- array(1:120, dim = 4:6)

# check dims order equivalence ====
expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, .abortcall = sys.call()),
  ci_sub(x, n(1:5, 1:4, 1:3), 3:1, FALSE, FALSE, .abortcall = sys.call())
)
expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, .abortcall = sys.call()),
  ci_sub(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, FALSE, .abortcall = sys.call())
)
expect_error(
  ci_sub(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, FALSE, .abortcall = sys.call()),
  pattern = "`dims` out of range",
  fixed = TRUE
)

expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, .abortcall = sys.call()),
  ci_sub(x, n(1:5, 1:4, 1:3), 3:1, FALSE, inv = TRUE, .abortcall = sys.call())
)
expect_equal(
  ci_sub(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, .abortcall = sys.call()),
  ci_sub(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, inv = TRUE, .abortcall = sys.call())
)
expect_error(
  ci_sub(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, inv = TRUE, .abortcall = sys.call()),
  pattern = "`dims` out of range",
  fixed = TRUE
)

enumerate <- enumerate + 9L


# check atomic vs list sub equivalence ====
expect_equal(
  ci_sub(x, n(1:4, 1:4, 1:4), 1:3, FALSE, FALSE, .abortcall = sys.call()),
  ci_sub(x, 1:4, 1:3, FALSE, FALSE, .abortcall = sys.call())
)

expect_equal(
  ci_sub(x, n(1:4), 1, FALSE, FALSE, .abortcall = sys.call()),
  ci_sub(x, 1:4, 1, FALSE, FALSE, .abortcall = sys.call())
)

enumerate <- enumerate + 2L

# error checking ====

