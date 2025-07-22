

# Virtually all of the functionality of ci_ii
# is tested through the tests in the generic method tests.

x <- array(1:120, dim = 4:6)
i <- cbind(1:3, 2:4, 3:5)

expect_error(
  ci_ii(x, i),
  pattern = "`i` must be a simple vector or function"
)

enumerate <- 1L
