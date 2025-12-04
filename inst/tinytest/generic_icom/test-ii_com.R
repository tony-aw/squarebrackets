
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0


x <- array(rnorm(100), c(5:3))
expect_equal(
  ii_icom(x, i = 1:2),
  1:2
)
dim(x) <- NULL
expect_equal(
  ii_icom(x, i = 1:2),
  1:2
)
enumerate <- enumerate + 2L
