
enumerate <- 0

# regular ====
indx <- 1:10
expect_equal(
  tci_int(indx, 11),
  indx
)
enumerate <- enumerate + 1


# inverse, length(indx) == 1 ====
indx <- 1
expect_equal(
  tci_int(indx, 11, -1),
  2:11
)
enumerate <- enumerate + 1


# inverse, n == 1 ====
indx <- 1
expect_equal(
  tci_int(indx, 1, -1),
  integer(0L)
)
enumerate <- enumerate + 1


# inverse, n == 2 ====
indx <- 1
expect_equal(
  tci_int(indx, 2, -1),
  2L
)
indx <- 2
expect_equal(
  tci_int(indx, 2, -1),
  1L
)
indx <- 1:2
expect_equal(
  tci_int(indx, 2, -1),
  integer(0L)
)
enumerate <- enumerate + 1



# inverse, n == 3 ====
n <- 3
indx <- 1
expect_equal(
  tci_int(indx, n, -1),
  2:n
)
indx <- 2
expect_equal(
  tci_int(indx, n, -1),
  c(1, n)
)
indx <- 1:2
expect_equal(
  tci_int(indx, n, -1),
  n
)

indx <- c(1, 3)
expect_equal(
  tci_int(indx, n, -1),
  2L
)
enumerate <- enumerate + 4L


# inverse, n == 4 ====
n <- 4
indx <- 1
expect_equal(
  tci_int(indx, n, -1),
  2:n
)
indx <- 2
expect_equal(
  tci_int(indx, n, -1),
  c(1, 3, n)
)
indx <- 1:3
expect_equal(
  tci_int(indx, n, -1),
  n
)

indx <- c(1, n)
expect_equal(
  tci_int(indx, n, -1),
  c(2, 3)
)
enumerate <- enumerate + 4L




# inverse ====
indx <- 1:10
expect_equal(
  tci_int(indx, 15, -1),
  11:15
)
expect_equal(
  tci_int(indx, 15, -1, chkdup = TRUE),
  11:15
)
enumerate <- enumerate + 2

