
enumerate <- 0

# unique indx, unique nms, no inv ====
indx <- c("b", "c")
nms <- letters
expect <- 2:3
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, uniquely_named = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE, uniquely_named = TRUE),
  expect
)
indx <- c("c", "b")
nms <- letters
expect <- 3:2
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, uniquely_named = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE, uniquely_named = TRUE),
  expect
)
enumerate <- enumerate + 8


# duplicate indx, unique nms, no inv ====
indx <- c("b", "b", "c")
nms <- letters
expect <- c(2, 2:3)
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, uniquely_named = TRUE),
  expect
)
indx <- c("c", "c", "b")
nms <- letters
expect <- c(3, 3:2)
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, uniquely_named = TRUE),
  expect
)

enumerate <- enumerate + 4


# unique indx, duplicate nms, no inv ====
indx <- c("b", "c")
nms <- c("b", letters)
expect <- c(1, 3:4)
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE),
  expect
)
indx <- c("c", "b")
nms <- c("b", letters)
expect <- c(4, 1, 3)
expect_equal(
  tci_chr(indx, nms),
  expect
)
expect_equal(
  tci_chr(indx, nms, chkdup = TRUE),
  expect
)

enumerate <- enumerate + 4



# unique indx, unique nms, inv ====
indx <- c("b", "c")
nms <- letters
expect <- c(1, 4:26)
expect_equal(
  tci_chr(indx, nms, -1),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, uniquely_named = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE, uniquely_named = TRUE),
  expect
)
indx <- c("c", "b")
nms <- letters
expect <- c(1, 4:26)
expect_equal(
  tci_chr(indx, nms, -1),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, uniquely_named = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE, uniquely_named = TRUE),
  expect
)
enumerate <- enumerate + 8


# unique indx, duplicate nms, inv ====
indx <- c("b", "c")
nms <- c("b", letters)
expect <- c(2, 5:27)
expect_equal(
  tci_chr(indx, nms, -1),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE),
  expect
)
indx <- c("c", "b")
nms <- c("b", letters)
expect <- c(2, 5:27)
expect_equal(
  tci_chr(indx, nms, -1),
  expect
)
expect_equal(
  tci_chr(indx, nms, -1, chkdup = TRUE),
  expect
)

enumerate <- enumerate + 4


# errors ====
nms <- letters
indx <- c("a", "a", "b")
expect_error(
  tci_chr(indx, NULL),
  pattern = "no names present"
)
expect_error(
  tci_chr(indx, nms, chkdup = TRUE),
  pattern = "duplicate integers or names not allowed"
)

