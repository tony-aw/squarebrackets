
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L
n <- 10
rel <- c(TRUE, FALSE)
from <- n(NULL, 1, 2, -1i, -2i)
start <- c(1, 1, 2, n, n - 1)
to <- n(NULL, -1i, -2i, 1, 2)
end <- c(n, n, n-1, 1, 2)



# general errors ====
expect_error(
  countv(~ form, v = 1L),
  pattern = "`y` must be atomic"
)
expect_error(
  countv(integer(0L), v= 1L),
  pattern = "zero-length `y` not allowed"
)
expect_error(
  countv(1, v = 1, r = 3),
  pattern = "`r` must be logical"
)
expect_error(
  countv(1, v = 1, r = c(TRUE, FALSE, TRUE)),
  pattern = "`length(r) > 2L`",
  fixed = TRUE
)
expect_error(
  countv(1, v = 1, r = NA),
  pattern = "`r` must not contain `NA`"
)
expect_error(
  countv(1, na = 0.5),
  pattern = "`na` must be `TRUE`, `FALSE`, or `NA`"
)
expect_error(
  countv(1, na = c(TRUE, FALSE)),
  pattern = "`na` must be `TRUE`, `FALSE`, or `NA`"
)
enumerate <- enumerate + 9L


# general value errors ====
expect_error(
  countv(1, v = ~form),
  pattern = "`v` must be atomic"
)
expect_error(
  countv(1, v = integer(0)),
  pattern = "zero-length `v` not allowed"
)
expect_error(
  countv(1, v = NA),
  pattern = "`v` must not contain NA/NaN; please use `na = NA` to find NA/NaN values"
)
enumerate <- enumerate + 3L


# errors for numeric y ====
y <- sample(c(1:10, NA, NaN, Inf, -Inf))
expect_error(
  countv(y, v = "a"),
  pattern = "if `y` is numeric, `v` must also be numeric"
)
expect_error(
  countv(y, v = 1:3),
  pattern = "if `y` is numeric `v` must be of length 1 or 2"
)
expect_error(
  countv(y, v = c(Inf, -Inf)),
  pattern = "problem in `v`: lower bound larger than upper bound"
)
expect_error(
  countv(y, v = 1:2, r = c(TRUE, FALSE)),
  pattern = "`r` must be Boolean"
)
enumerate <- enumerate + 4L



# errors for character y ====
y <- sample(letters)
expect_error(
  countv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(y)`",
  fixed = TRUE
)
expect_error(
  countv(y, v = ""),
  pattern = "`v` cannot contain zero-length strings",
  fixed = TRUE
)
expect_error(
  countv(y, v = "a", r = c(TRUE, FALSE)),
  pattern = "`r` must be Boolean"
)
enumerate <- enumerate + 3L


# errors for logical y ====
y <- c(TRUE, FALSE, NA)
expect_error(
  countv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(y)`",
  fixed = TRUE
)
expect_error(
  countv(y, v = c(TRUE, FALSE)),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# errors for raw y ====
y <- as.raw(1:10)
expect_error(
  countv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(y)`",
  fixed = TRUE
)
expect_error(
  countv(y, v = as.raw(1:2)),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L



# errors for complex y ====
y <- 1:10 + 1:10 * -1i
expect_error(
  countv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(y)`",
  fixed = TRUE
)
expect_error(
  countv(y, v = y[1:3]),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L



# errors for factor y ====
y <- factor(c(letters, NA), exclude = NULL)
expect_error(
  countv(y, v = 1L),
  pattern = "`NA` factor-levels not supported",
  fixed = TRUE
)
y <- factor(letters)
expect_error(
  countv(y, v = 1:2),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L

