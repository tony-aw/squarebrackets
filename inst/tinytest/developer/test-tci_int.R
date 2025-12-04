
enumerate <- 0

# regular ====
indx <- 1:10
expect_equal(
  tci_int(indx, 11),
  indx
)
enumerate <- enumerate + 1

# inverse ====
indx <- 1:10
expect_equal(
  tci_int(indx, 15, inv = TRUE),
  11:15
)
expect_equal(
  tci_int(indx, 15, inv = TRUE, chkdup = TRUE),
  11:15
)
enumerate <- enumerate + 2

# errors ====
expect_error(
  tci_int(1:10, 9),
  pattern = "integers must be >= 1 and <= bounds"
)
expect_error(
  tci_int(c(1, 1, 2:3), 9, chkdup = TRUE),
  pattern = "duplicate integers or names not allowed"
)
enumerate <- enumerate + 2


