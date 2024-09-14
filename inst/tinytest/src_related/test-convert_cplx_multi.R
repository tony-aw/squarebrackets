
enumerate <- 0

x <- c(1 + 1i, 1 - 1i)
expect_equal(
  squarebrackets:::.rcpp_indx_convert_cplx_multi(Re(x), Im(x), c(10, 10)),
  c(1, 10)
)

expect_error(
  squarebrackets:::.rcpp_indx_convert_cplx_multi(Re(x), Im(x), 10)
)

enumerate <- enumerate + 2
