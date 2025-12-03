
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
  pattern = "`use` out of range",
  fixed = TRUE
)

expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), -1:-3, FALSE, .abortcall = sys.call()),
  ci_ss(x, n(1:5, 1:4, 1:3), -3:-1, FALSE, .abortcall = sys.call())
)
expect_equal(
  ci_ss(x, n(1:3, 1:4, 1:5), -1:-3, FALSE, .abortcall = sys.call()),
  ci_ss(x, n(1:4, 1:5, 1:3), c(-2, -3, -1), FALSE, .abortcall = sys.call())
)
expect_error(
  ci_ss(x, n(1:3, 1:4, 1:5, 1:6), -1:-4, FALSE, .abortcall = sys.call()),
  pattern = "`use` out of range",
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
  ci_ss(x, list(1:3, 0), -Inf),
  ci_ss(x, list(1:3, NULL), -Inf)
) |> errorfun()
expect_equal(
  ci_ss(x, list(0, 1:3), -Inf),
  ci_ss(x, list(NULL, 1:3), -Inf)
) |> errorfun()

expect_equal(
  ci_ss(x, list(0)),
  ci_ss(x, list(NULL))
) |> errorfun()
expect_equal(
  ci_ss(x, list(0), -Inf),
  ci_ss(x, list(NULL), -Inf)
) |> errorfun()

expect_equal(
  ci_ss(x, 0),
  ci_ss(x, NULL)
) |> errorfun()
expect_equal(
  ci_ss(x, 0, -Inf),
  ci_ss(x, NULL, -Inf)
) |> errorfun()

enumerate <- enumerate + 8L


# check zero-length argument equivalence
x <- matrix(1:20, ncol = 4)
expect_equal(
  ci_ss(x, list(1:3, integer(0L))),
  ci_ss(x, list(1:3, logical(0L)))
) |> errorfun()
expect_equal(
  ci_ss(x, list(integer(0L), 1:3)),
  ci_ss(x, list(logical(0L), 1:3))
) |> errorfun()
expect_equal(
  ci_ss(x, list(1:3, integer(0L)), -Inf),
  ci_ss(x, list(1:3, logical(0L)), -Inf)
) |> errorfun()

expect_equal(
  ci_ss(x, list(integer(0L))),
  ci_ss(x, integer(0L))
) |> errorfun()
expect_equal(
  ci_ss(x, list(integer(0L)), -Inf),
  ci_ss(x, integer(0L), -Inf)
) |> errorfun()


enumerate <- enumerate + 8L




# general error tests ====
expect_error(
  ci_ss(x, 1:3, "a")
)
expect_error(
  ci_ss(x, n(1:3, 1:3, 1:3), 1:2),
  pattern = "`length(s)` must be 1 or equal to `length(use)`",
  fixed = TRUE
)
expect_error(
  ci_ss(x, 1:3, 1:10),
  pattern = "`use` out of range"
)
expect_error(
  ci_ss(x, 1:3, integer(0L)),
  pattern = "length(use) == 0L has not been captured",
  fixed = TRUE
)

enumerate <- enumerate + 4L

