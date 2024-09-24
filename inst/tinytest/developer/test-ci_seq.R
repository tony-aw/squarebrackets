
# note: majority of tests for ci_seq() are done through the tests for idx_r()

enumerate <- 0


# basic checks ====
x <- 1:10
expect_equal(
  ci_seq(x, 0L, 1L, 5L),
  list(start = 1L, end = 5L, by = 1L, length.out =  5L)
)
expect_equal(
  ci_seq(x, 0L, 5, 1),
  list(start = 5L, end = 1L, by = -1L, length.out =  5L)
)

expect_equal(
  ci_seq(x, 0L, 1, 5, 2),
  list(start = 1L, end = 5L, by = 2L, length.out =  3L)
)

expect_equal(
  ci_seq(x, 0L, 6, 1, 2),
  list(start = 6L, end = 2L, by = -2L, length.out =  3L)
)

enumerate <- enumerate + 4


# error checks - bad `m`:
x <- 1:10

expect_error(
  ci_seq(x, "q"),
  pattern = "`m` must be (complex) numeric",
  fixed = TRUE
)

expect_error(
  ci_seq(x, -1),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, -1 -1i),
  pattern = "index out of bounds"
)

x <- matrix(1:10, ncol = 2)

expect_error(
  ci_seq(x, c(0, 1)),
  pattern = "improper `m` given",
  fixed = TRUE
)
enumerate <- enumerate + 4


# error checks - bad by:
x <- 1:10
expect_error(
  ci_seq(x, by = NA_integer_),
  pattern = "`by` cannot be `NA`"
)
expect_error(
  ci_seq(x, by = 0L),
  pattern = "`by` cannot be zero"
)
expect_error(
  ci_seq(x, by = 11.5),
  pattern = "`by` cannot be fractional"
)

enumerate <- enumerate + 3



# error checks - bad lengths:
x <- matrix(1:10, ncol = 2)

expect_error(
  ci_seq(x, 0, c(1, 2), 2, 1),
  pattern = "`m`, `start`, `end` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, 1, c(1, 2), 1),
  pattern = "`m`, `start`, `end` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, 1, 2,  c(1, 2)),
  pattern = "`m`, `start`, `end` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)
enumerate <- enumerate + 3


# error checks - bas args:
x <- matrix(1:10, ncol = 2)

expect_error(
  ci_seq(x, 0, start = 1, by = 1),
  pattern = "either specify both `start` and `end`, or specify neither",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, end = 1, by = 1),
  pattern = "either specify both `start` and `end`, or specify neither",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, by = NULL),
  pattern = "`by` missing",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, start = "a", end = 3),
  pattern = "`m`, `start`, `end` `by` must be (complex) numeric or `NULL`",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, start = 1, end = "z"),
  pattern = "`m`, `start`, `end` `by` must be (complex) numeric or `NULL`",
  fixed = TRUE
)

expect_error(
  ci_seq(x, 0, by = 0),
  pattern = "`by` cannot be zero",
  fixed = TRUE
)
enumerate <- enumerate + 6


# error checks - bad start/end:
x <- matrix(1:10, ncol = 2)

expect_error(
  ci_seq(x, 1, start = 1, end = 6),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, 1, start = -1, end = 5),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, 1, start = 6 -1i, end = 6),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, 1, start = NA_integer_, end = 4),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, 1, start = 1L, end = NA_integer_),
  pattern = "index out of bounds"
)

expect_error(
  ci_seq(x, 1, start = 1, end = 6 -1i),
  pattern = "index out of bounds"
)

enumerate <- enumerate + 6


