
# set-up ====
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L
n <- 10
rel <- c(TRUE, FALSE)



# general errors ====
expect_error(
  stride_pv(~ form, v = 1L),
  pattern = "`p` must be atomic"
)
expect_error(
  stride_pv(integer(0L), v= 1L),
  pattern = "zero-length `p` not allowed"
)
expect_error(
  stride_pv(1, na = 0.5),
  pattern = "`na` must be `TRUE`, `FALSE`, or `NA`"
)
expect_error(
  stride_pv(1, na = c(TRUE, FALSE)),
  pattern = "`na` must be `TRUE`, `FALSE`, or `NA`"
)
enumerate <- enumerate + 4L


# general value errors ====
expect_error(
  stride_pv(1, v = ~form),
  pattern = "`v` must be atomic"
)
expect_error(
  stride_pv(1, v = integer(0)),
  pattern = "zero-length `v` not allowed"
)
expect_error(
  stride_pv(1, v = NA),
  pattern = "`v` must not contain NA/NaN; please use `na = NA` to find NA/NaN values"
)
enumerate <- enumerate + 3L


# errors for numeric y ====
y <- sample(c(1:10, NA, NaN, Inf, -Inf))
expect_error(
  stride_pv(y, v = "a"),
  pattern = "if `p` is numeric, `v` must also be numeric"
)
expect_error(
  stride_pv(y, v = 1:3),
  pattern = "if `p` is numeric `v` must be of length 1 or 2"
)
expect_error(
  stride_pv(y, v = c(Inf, -Inf)),
  pattern = "problem in `v`: lower bound larger than upper bound"
)
enumerate <- enumerate + 3L



# errors for character y ====
y <- sample(letters)
expect_error(
  stride_pv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(p)`",
  fixed = TRUE
)
expect_error(
  stride_pv(y, v = ""),
  pattern = "`v` cannot contain zero-length strings",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# errors for logical y ====
y <- c(TRUE, FALSE, NA)
expect_error(
  stride_pv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(p)`",
  fixed = TRUE
)
expect_error(
  stride_pv(y, v = c(TRUE, FALSE)),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# errors for raw y ====
y <- as.raw(1:10)
expect_error(
  stride_pv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(p)`",
  fixed = TRUE
)
expect_error(
  stride_pv(y, v = as.raw(1:2)),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L



# errors for complex y ====
y <- (1:10 + 1:10 * -1i)
expect_error(
  stride_pv(y, v = 1L),
  pattern = "`typeof(v)` not compatible with `typeof(p)`",
  fixed = TRUE
)
expect_error(
  stride_pv(y, v = y[1:3]),
  pattern = "non-scalar `v` not supported for this data type",
  fixed = TRUE
)
enumerate <- enumerate + 2L


# test warnings ====
expect_message(
  stride_pv(y, v = 1L, na = NA),
  pattern = "`na = NA`, so argument `v` will be ignored"
)

