
# generic dots ====

x <- mutatomic::mutatomic(1:10)
expect_error(
  idx(x, foo = TRUE),
  pattern = "unknown arguments given"
)

x <- mutatomic::mutatomic(1:20, dim = c(4,5))
expect_error(
  idx(x, foo = TRUE),
  pattern = "unknown arguments given"
)

x <- mutatomic::mutatomic(1:27, dim = c(3,3,3))
expect_error(
  idx(x, foo = TRUE),
  pattern = "unknown arguments given"
)

x <- factor(letters)
expect_error(
  idx(x, foo = TRUE),
  pattern = "unknown arguments given"
)

enumerate <- 4L
