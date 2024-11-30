
enumerate <- 0

.C_match_range <- squarebrackets:::.C_match_range

x <- sample(c(1:10, rep(NA, 9)))
n <- sum(!is.na(x))
expect <- order(x)[1:n]
expect_equal(
  expect,
  .C_match_range(order(x), x)
)

x <- sample(c(1:20, rep(NA, 11)))
n <- sum(!is.na(x))
expect <- order(x)[1:n]
expect_equal(
  expect,
  .C_match_range(order(x), x)
)


x <- sample(c(1:1000, rep(NA, 1999)))
n <- sum(!is.na(x))
expect <- order(x)[1:n]
expect_equal(
  expect,
  .C_match_range(order(x), x)
)

enumerate <- enumerate + 3L
