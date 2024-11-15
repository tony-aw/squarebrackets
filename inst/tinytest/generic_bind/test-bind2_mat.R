
# set-up ====

source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0
temp.fun <- function(x, ...) {
  return(x[...])
}
enumerate <- 0

# test vectors with vectors ====
expect_equal(
  bind_mat(n(as.list(1:10), as.list(1:10)), along = 1L),
  rbind(as.list(1:10), as.list(1:10)) 
)
expect_equal(
  bind_mat(n(as.list(1:10), as.list(1:10)), along = 2L),
  cbind(as.list(1:10), as.list(1:10)) 
)
enumerate <- enumerate + 2L

# test vectors with matrices ====
expect_equal(
  bind_mat(n(as.list(1:10), matrix(as.list(1:20), nrow = 2)), along = 1L),
  rbind(as.list(1:10), matrix(as.list(1:20), nrow= 2)) 
)
expect_equal(
  bind_mat(n(as.list(1:10), matrix(as.list(1:20), ncol = 2)), along = 2L),
  cbind(as.list(1:10), matrix(as.list(1:20), ncol = 2)) 
)
enumerate <- enumerate + 2L

# test matrices with matrices ====
expect_equal(
  bind_mat(n(matrix(as.list(1:20), nrow = 2), matrix(as.list(1:20), nrow = 2)), along = 1L),
  rbind(matrix(as.list(1:20), nrow = 2), matrix(as.list(1:20), nrow= 2)) 
)
expect_equal(
  bind_mat(n(matrix(as.list(1:20), ncol = 2), matrix(as.list(1:20), ncol = 2)), along = 2L),
  cbind(matrix(as.list(1:20), ncol = 2), matrix(as.list(1:20), ncol = 2)) 
)
enumerate <- enumerate + 2L


# test errors ====
x <- matrix(as.list(1:20), ncol = 2)
y <- matrix(1:30, ncol = 3)
expect_error(
  bind_mat(n(x, y), along = 1),
  pattern = "fractional recycling not allowed"
)
x <- matrix(as.list(1:20), nrow = 2)
y <- matrix(1:30, nrow = 3)
expect_error(
  bind_mat(n(x,y), along = 2),
  pattern = "fractional recycling not allowed"
)
enumerate <- enumerate + 2

