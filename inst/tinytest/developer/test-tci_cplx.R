
enumerate <- 0

# regular ====
indx <- 1:10 * 1i
expect_equal(
  tci_cplx(indx, 11),
  1:10
)
indx <- 1:10 * -1i
expect_equal(
  tci_cplx(indx, 11),
  11:2
)
enumerate <- enumerate + 2


# inverse ====
indx <- 1:10 * 1i
expect_equal(
  tci_cplx(indx, 15, inv = TRUE),
  11:15
)
inx <- 1:10 * -1i
expect_equal(
  tci_cplx(indx, 15, inv = TRUE, chkdup = TRUE),
  11:15
)
enumerate <- enumerate + 2



# errors ====
expect_error(
  tci_cplx(1:10 * 1i, 9),
  pattern = "integers must be >= 1 and <= bounds"
)
expect_error(
  tci_cplx(c(1, 1, 2:3) * 1i, 9, chkdup = TRUE),
  pattern = "duplicate integers or names not allowed"
)
expect_error(
  tci_cplx(c(2, -1) * 1i, 2, chkdup = TRUE),
  pattern = "duplicate integers or names not allowed"
)
indx <- 1:10 + 0i
expect_error(
  tci_cplx(indx, 11)
)
indx <- 1:10 + 0i
expect_error(
  tci_cplx(indx, 15, inv = TRUE)
)
enumerate <- enumerate + 5


