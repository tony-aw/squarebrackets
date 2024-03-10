
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


# lists ====
x <- list(a = seq(1.5, 10.5, by = 1), b = letters)
expect_equal(
  sb_coe(x, 1, as.integer),
  {x[[1]] <- as.integer(x[[1]]); x}
)


# data.frame-like ====
obj <- data.table::data.table(
  a = seq(1.5, 10.5, by = 1),
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj1 <- data.table::data.table(
  a = as.integer(1:10),
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj2 <- sb_coe(obj, vars = is.numeric, v = as.integer)
expect_equal(
  obj1,
  obj2
)


enumerate <- enumerate + 5