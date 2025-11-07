
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
  tci_int(indx, 11, inv = TRUE),
  2:11
)
enumerate <- enumerate + 1


# inverse, n == 1 ====
indx <- 1
expect_equal(
  tci_int(indx, 1, inv = TRUE),
  integer(0L)
)
enumerate <- enumerate + 1


# inverse, n == 2 ====
indx <- 1
expect_equal(
  tci_int(indx, 2, inv = TRUE),
  2L
)
indx <- 2
expect_equal(
  tci_int(indx, 2, inv = TRUE),
  1L
)
indx <- 1:2
expect_equal(
  tci_int(indx, 2, inv = TRUE),
  integer(0L)
)
enumerate <- enumerate + 1



# inverse, n == 3 ====
n <- 3
indx <- 1
expect_equal(
  tci_int(indx, n, inv = TRUE),
  2:n
)
indx <- 2
expect_equal(
  tci_int(indx, n, inv = TRUE),
  c(1, n)
)
indx <- 1:2
expect_equal(
  tci_int(indx, n, inv = TRUE),
  n
)

indx <- c(1, 3)
expect_equal(
  tci_int(indx, n, inv = TRUE),
  2L
)
enumerate <- enumerate + 4L


# inverse, n == 4 ====
n <- 4
indx <- 1
expect_equal(
  tci_int(indx, n, inv = TRUE),
  2:n
)
indx <- 2
expect_equal(
  tci_int(indx, n, inv = TRUE),
  c(1, 3, n)
)
indx <- 1:3
expect_equal(
  tci_int(indx, n, inv = TRUE),
  n
)

indx <- c(1, n)
expect_equal(
  tci_int(indx, n, inv = TRUE),
  c(2, 3)
)
enumerate <- enumerate + 4L




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
