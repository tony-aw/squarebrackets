
# note: majority of tests for cp_seq() are done through the tests for idx_r()

enumerate <- 0


# basic checks ====
x <- 1:10
expect_equal(
  cp_seq(x, 0L, 1L, 5L),
  list(start = 1L, end = 5L, by = 1L, length.out =  5L)
)
expect_equal(
  cp_seq(x, 0L, 5, 1),
  list(start = 5L, end = 1L, by = -1L, length.out =  5L)
)

expect_equal(
  cp_seq(x, 0L, 1, 5, 2),
  list(start = 1L, end = 5L, by = 2L, length.out =  3L)
)

expect_equal(
  cp_seq(x, 0L, 6, 1, 2),
  list(start = 6L, end = 2L, by = -2L, length.out =  3L)
)

enumerate <- enumerate + 4


# error checks - bad `m`:
x <- 1:10

expect_error(
  cp_seq(x, "q"),
  pattern = "`m` must be (complex) numeric",
  fixed = TRUE
)

expect_error(
  cp_seq(x, -1),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 100 * -1i),
  pattern = "index out of bounds"
)

x <- matrix(1:10, ncol = 2)

expect_error(
  cp_seq(x, c(0, 1)),
  pattern = "improper `m` given",
  fixed = TRUE
)
enumerate <- enumerate + 4


# error checks - bad by:
x <- 1:10
expect_error(
  cp_seq(x, by = NA_integer_),
  pattern = "`by` cannot be `NA`"
)
expect_error(
  cp_seq(x, by = 0L),
  pattern = "`by` cannot be zero"
)
expect_error(
  cp_seq(x, by = 11.5),
  pattern = "`by` cannot be fractional"
)

enumerate <- enumerate + 3



# error checks - bad lengths:
x <- matrix(1:10, ncol = 2)

expect_error(
  cp_seq(x, 0, c(1, 2), 2, 1),
  pattern = "`m`, `from`, `to` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, 1, c(1, 2), 1),
  pattern = "`m`, `from`, `to` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, 1, 2,  c(1, 2)),
  pattern = "`m`, `from`, `to` `by` must be equal length, length of 1, or `NULL`",
  fixed = TRUE
)
enumerate <- enumerate + 3


# error checks - bas args:
x <- matrix(1:10, ncol = 2)

expect_error(
  cp_seq(x, 0, from = 1, by = 1),
  pattern = "either specify both `from` and `to`, or specify neither",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, to = 1, by = 1),
  pattern = "either specify both `from` and `to`, or specify neither",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, by = NULL),
  pattern = "`by` missing",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, from = "a", to = 3),
  pattern = "`m`, `from`, `to` `by` must be (complex) numeric or `NULL`",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, from = 1, to = "z"),
  pattern = "`m`, `from`, `to` `by` must be (complex) numeric or `NULL`",
  fixed = TRUE
)

expect_error(
  cp_seq(x, 0, by = 0),
  pattern = "`by` cannot be zero",
  fixed = TRUE
)
enumerate <- enumerate + 6


# error checks - bad start/end:
x <- matrix(1:10, ncol = 2)

expect_error(
  cp_seq(x, 1, from = 1, to = 6),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 1, from = -1, to = 5),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 1, from = 6 * -1i, to = 6),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 1, from = NA_integer_, to = 4),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 1, from = 1L, to = NA_integer_),
  pattern = "index out of bounds"
)

expect_error(
  cp_seq(x, 1, from = 1, to = 6 * -1i),
  pattern = "index out of bounds"
)

enumerate <- enumerate + 6


