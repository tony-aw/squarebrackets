
x <- letters
expect_true(
  squarebrackets:::.rcpp_address(x) %in% squarebrackets:::.protected_addresses()
)

x <- 1
expect_false(
  squarebrackets:::.rcpp_address(x) %in% squarebrackets:::.protected_addresses()
)
