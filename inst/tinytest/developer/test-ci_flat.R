

# Virtually all of the functionality of ci_flat
# is tested through the tests in the generic method tests.

x <- array(1:120, dim = 4:6)
i <- cbind(1:3, 2:4, 3:5)

expect_error(
  ci_flat(x, i),
  pattern = "coordinate matrix not supported for `i`"
)

enumerate <- 1L
