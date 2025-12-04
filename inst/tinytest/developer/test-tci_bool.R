
# regular ====
indx <- sample(c(TRUE, FALSE), 10, TRUE)
expect_equal(
  tci_bool(indx, length(indx)),
  which(indx)
)

# inverse ====
indx <- sample(c(TRUE, FALSE), 10, TRUE)
expect_equal(
  tci_bool(indx, length(indx), inv = TRUE),
  which(!indx)
)

# errors ====
expect_error(
  tci_bool(indx, length(indx) + 1L),
  pattern = "incorrect length of logical indices"
)

enumerate <- 3
