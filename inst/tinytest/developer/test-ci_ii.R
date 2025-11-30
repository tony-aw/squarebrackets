

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


# check index type casting ====
# these are all equivalent to selecting the first and last layer
x <- setNames(sample(1:6), letters[1:6])
indices <- list(
  c(TRUE, rep(FALSE, 4), TRUE),
  c(1L, 6L),
  c(1.0, 6.0),
  c("a", "f")
)
expected <- c(1, 6)
for(i in seq_along(indices)) {
  expect_equal(
    ci_ii(x, indices[[i]]),
    expected
  ) |> errorfun()
  enumerate <- enumerate + 1L
}


# error tests ====

# Virtually all of the functionality of ci_ii
# is tested through the tests in the generic method tests.

x <- array(1:120, dim = 4:6)
i <- cbind(1:3, 2:4, 3:5)


expect_error(
  ci_ii(x, i),
  pattern = "`i` must be a simple vector"
)

enumerate <- 1L
