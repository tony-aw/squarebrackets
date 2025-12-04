
enumerate <- 0

x <- c(1 * 1i, 1 * -1i)
expect_equal(
  squarebrackets:::.C_convert_cplx(x, 10),
  c(1, 10)
)
x <- c(1 * 1i, 1 * -1i)
expect_equal(
  squarebrackets:::.C_convert_cplx(x, c(10, 9)),
  c(1, 9)
)

enumerate <- enumerate + 2
