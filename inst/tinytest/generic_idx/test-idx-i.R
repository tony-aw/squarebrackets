
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0


x <- array(rnorm(100), c(5:3))
expect_equal(
  idx(x, i = 1:2 * -1i),
  length(x) - 0:1
)
dim(x) <- NULL
expect_equal(
  idx(x, i = 1:2 * -1i),
  length(x) - 0:1
)
enumerate <- enumerate + 2L
