
x <- 1:10
names(x) <- letters[1:10]
attr(x, "test") <- "test"
x2 <- x[1:5]
attr(x2, "test") <- "test"

expect_equal(
  sb_x(x, 1:5, rat = TRUE),
  x2
)
expect_equal(
  sb_rm(x, 6:10, rat = TRUE),
  x2
)

x <- matrix(1:20, ncol = 4)
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- letters[1:5]
attr(x, "test") <- "test"
x2 <- x[1:2, 1:2]
attr(x2, "test") <- "test"

expect_equal(
  sb_x(x, 1:2, 1:2, rat = TRUE),
  x2
)
expect_equal(
  sb_rm(x, 3:5, 3:4, rat = TRUE),
  x2
)

dims <- c(4,4,4)
x <- array(1:prod(dims), dim =dims)
nms <- c("a", "b", 'c', "d")
dimnames(x) <- list(nms, nms, nms)
attr(x, "test") <- "test"
x2 <- x[1:2, 1:2, 1:2]
attr(x2, "test") <- "test"

expect_equal(
  sb_x(x, list(1:2, 1:2, 1:2), 1:3, rat = TRUE),
  x2
)
expect_equal(
  sb_rm(x, list(3:4, 3:4, 3:4), 1:3, rat = TRUE),
  x2
)

x <- as.list(1:10)
names(x) <- letters[1:10]
attr(x, "test") <- "test"
x2 <- x[1:5]
attr(x2, "test") <- "test"

expect_equal(
  sb2_x(x, 1:5, rat = TRUE),
  x2
)
expect_equal(
  sb2_rm(x, 6:10, rat = TRUE),
  x2
)

enumerate <- 8

