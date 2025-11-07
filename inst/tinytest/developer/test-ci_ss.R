
enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


x <- array(1:120, dim = 4:6)
dimnames(x)[[3]] <- letters[1:6]

# check index type casting ====
# these are all equivalent to selecting the first and last layer
indices <- list(
  c(TRUE, rep(FALSE, 4), TRUE),
  c(1L, 6L),
  c(1.0, 6.0),
  c(1, -1) * 1i,
  c("a", "f")
)
expected <- list(1:4, 1:5, c(1, 6))
for(i in seq_along(indices)) {
  expect_equal(
    ci_ss(x, indices[[i]], 3),
    expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


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


# check NULL/0L equivalence ====
x <- matrix(1:20, ncol = 4)
expect_equal(
  ci_ss(x, list(1:3, 0)),
  ci_ss(x, list(1:3, NULL))
) |> errorfun()
expect_equal(
  ci_ss(x, list(0, 1:3)),
  ci_ss(x, list(NULL, 1:3))
) |> errorfun()
expect_equal(
  ci_ss(x, list(1:3, 0), inv = TRUE),
  ci_ss(x, list(1:3, NULL), inv = TRUE)
) |> errorfun()
expect_equal(
  ci_ss(x, list(0, 1:3), inv = TRUE),
  ci_ss(x, list(NULL, 1:3), inv = TRUE)
) |> errorfun()

expect_equal(
  ci_ss(x, list(0)),
  ci_ss(x, list(NULL))
) |> errorfun()
expect_equal(
  ci_ss(x, list(0), inv = TRUE),
  ci_ss(x, list(NULL), inv = TRUE)
) |> errorfun()

expect_equal(
  ci_ss(x, 0),
  ci_ss(x, NULL)
) |> errorfun()
expect_equal(
  ci_ss(x, 0, inv = TRUE),
  ci_ss(x, NULL, inv = TRUE)
) |> errorfun()

enumerate <- enumerate + 8L



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
expect_error(
  ci_ss(x, 1:3, integer(0L)),
  pattern = "length(d) == 0L has not been captured",
  fixed = TRUE
)

enumerate <- enumerate + 4L

