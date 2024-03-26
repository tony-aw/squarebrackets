
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# default method ====
x <- seq(1.5, 10.5, by = 1)
expect_equal(
  sb_coe(x, as.integer),
  {x[] <- as.integer(x); print(x)}
)


# default method - mutable atomic ====
x <- as.mutable_atomic(seq(1.5, 10.5, by = 1))
expect_equal(
  sb_coe(x, as.integer),
  {x[] <- as.integer(x); x}
)


# factors ====
x <- as.factor(letters)
expect_equal(
  sb_coe(x, as.character),
  as.character(x)
)


enumerate <- enumerate + 3
