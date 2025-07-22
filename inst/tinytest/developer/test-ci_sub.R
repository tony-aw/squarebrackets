

enumerate <- 0L

x <- array(1:120, dim = 4:6)


# check dims order equivalence ====
expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, .abortcall = sys.call()),
  ci_ss(x, n(1:5, 1:4, 1:3), 3:1, FALSE, FALSE, .abortcall = sys.call())
)
expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), 1:3, FALSE, FALSE, .abortcall = sys.call()),
  ci_ss(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, FALSE, .abortcall = sys.call())
)
expect_error(
  ci_ss(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, FALSE, .abortcall = sys.call()),
  pattern = "`d` out of range",
  fixed = TRUE
)

expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, .abortcall = sys.call()),
  ci_ss(x, n(1:5, 1:4, 1:3), 3:1, FALSE, inv = TRUE, .abortcall = sys.call())
)
expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), 1:3, FALSE, inv = TRUE, .abortcall = sys.call()),
  ci_ss(x, n(1:4, 1:5, 1:3), c(2, 3, 1), FALSE, inv = TRUE, .abortcall = sys.call())
)
expect_error(
  ci_ss(x, n(1:3, 1:4, 1:5, 1:6), 1:4, FALSE, inv = TRUE, .abortcall = sys.call()),
  pattern = "`d` out of range",
  fixed = TRUE
)

enumerate <- enumerate + 6L


# atomic s tests ====
expect_equal(
  ci_ss(x, n(1:4), 1),
  ci_ss(x, 1:4, 1)
)
expect_equal(
  ci_ss(x, n(1:4, 1:4), 1:2),
  ci_ss(x, 1:4, 1:2)
)
enumerate <- enumerate + 2L


# general error tests ====
expect_error(
  ci_ss(x, 1:3, "a"),
  pattern = "`d` must be a integer vector"
)
expect_error(
  ci_ss(x, n(1:3, 1:3, 1:3), 1:2),
  pattern = "`length(s)` must equal `length(d)`",
  fixed = TRUE
)
expect_error(
  ci_ss(x, 1:3, 1:10),
  pattern = "`d` out of range"
)

enumerate <- enumerate + 3L

