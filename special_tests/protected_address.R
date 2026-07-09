# protected address ====
x <- base::letters
data.table::setattr(x, "class", "mutatomic")
data.table::setattr(x, "serial", squarebrackets:::.C_serial(x))
expect_true(
  .rcpp_address(x) %in% squarebrackets:::.pkgenv_mutatomic[["protected"]]
)
expect_error(
  ii_set(x, 1, rp = "xxx")
)
expect_error(
  long_set(x, stride_v(x, v = "a"), rp = "xxx")
)

enumerate <- enumerate + 1L
