
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# is.mutable_atomic ====
x <- 0:10
expect_false(
  is.mutable_atomic(as.logical(x))
)
expect_false(
  is.mutable_atomic(as.integer(x))
)
expect_false(
  is.mutable_atomic(as.double(x))
)
expect_false(
  is.mutable_atomic(as.character(x))
)
expect_false(
  is.mutable_atomic(as.complex(x))
)
expect_false(
  is.mutable_atomic(as.raw(x))
)

x <- as.mutable_atomic(x)
expect_true(
  is.mutable_atomic(x)
)

x <- factor(letters)
class(x) <- "mutable_atomic"
attr(x, 'typeof') <- typeof(x)
expect_false(
  is.mutable_atomic(as.character(x))
)

enumerate <- enumerate + 8


# couldb.mutable_atomic ====
x <- 0:10
expect_true(
  couldb.mutable_atomic(as.logical(x))
)
expect_true(
  couldb.mutable_atomic(as.integer(x))
)
expect_true(
  couldb.mutable_atomic(as.double(x))
)
expect_true(
  couldb.mutable_atomic(as.character(x))
)
expect_true(
  couldb.mutable_atomic(as.complex(x))
)
expect_true(
  couldb.mutable_atomic(as.raw(x))
)

enumerate <- enumerate + 6


# as.mutable_atomic vs mutable_atomic - vector ====
x <- 1:10
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:10)
)
names(x) <- letters[1:10]
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:10, names = letters[1:10])
)
enumerate <- enumerate + 2


# as.mutable_atomic vs mutable_atomic - matrix ====
x <- matrix(1:20, ncol = 4)
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:20, dim = c(5, 4))
)
dimnames(x) <- n(NULL, letters[1:4])
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(x, dim = c(5, 4), dimnames = n(NULL, letters[1:4]))
)

x <- matrix(1:20, ncol = 4)
names(x) <- letters[1:20]
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(x, dim = c(5, 4), names = letters[1:20])
)
dimnames(x) <- n(NULL, letters[1:4])
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(x, dim = c(5, 4), names = letters[1:20], dimnames = n(NULL, letters[1:4]))
)

enumerate <- enumerate + 4


# as.mutable_atomic vs mutable_atomic - array ====
x <- array(1:27, dim = c(3,3,3))
dimnames(x) <- n(NULL, NULL, letters[1:3])
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:27, dim = c(3,3,3), dimnames = n(NULL, NULL, letters[1:3]))
)
names(x) <- c(letters, NA)
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:27, dim = c(3,3,3), dimnames = n(NULL, NULL, letters[1:3]), names = c(letters, NA))
)

enumerate <- enumerate + 3



# as.mutable_atomic vs mutable_atomic vs materialize_atomic - ALTREP ====
x <- 1:1e6
is_altrep <- squarebrackets:::.C_is_altrep
expect_true(is_altrep(x))
expect_false(as.mutable_atomic(x) |> is_altrep())
expect_false(mutable_atomic(x) |> is_altrep())
enumerate <- enumerate + 2L


# partial matrix sub-setting ====
x <- as.mutable_atomic(matrix(1:20, ncol = 4))
expect_equal(
  sb_x(x, 1, 1),
  x[1, ,drop = FALSE]
)
expect_equal(
  sb_x(x, 1, 2),
  x[, 1, drop = FALSE]
)

expect_equal(
  sb_wo(x, 1, 1),
  x[-1, ,drop = FALSE]
)
expect_equal(
  sb_wo(x, 1, 2),
  x[, -1, drop = FALSE]
)
enumerate <- enumerate + 4


# partial (n=4)-dim array sub-setting ====
x <- as.mutable_atomic(array(1:81, dim = c(3,3,3, 3)))
expect_equal(
  sb_x(x, sub = 1, dims = 1),
  x[1, , ,,drop = FALSE]
)
expect_equal(
  sb_x(x, sub = 1, dims = 2),
  x[, 1, ,, drop = FALSE]
)
expect_equal(
  sb_x(x, sub = 1, dims = 3),
  x[, , 1 ,, drop = FALSE]
)
expect_equal(
  sb_x(x, sub = 1, dims = 4),
  x[, , ,1, drop = FALSE]
)

expect_equal(
  sb_wo(x, sub = 1, dims = 1),
  x[-1, , ,,drop = FALSE]
)
expect_equal(
  sb_wo(x, sub = 1, dims = 2),
  x[, -1, ,, drop = FALSE]
)
expect_equal(
  sb_wo(x, sub = 1, dims = 3),
  x[, , -1 ,, drop = FALSE]
)
expect_equal(
  sb_wo(x, sub = 1, dims = 4),
  x[, , ,-1, drop = FALSE]
)

enumerate <- enumerate + 8


# errors ====

expect_error(
  mutable_atomic(list(1:10)),
  pattern = "non-atomic data given"
)
expect_error(
  as.mutable_atomic(list(1:10)),
  "not atomic"
)
# expect_error(
#   mutable_atomic(factor(letters)),
#   "factors cannot be mutable"
# )
# expect_error(
#   as.mutable_atomic(factor(letters)),
#   "factors cannot be mutable"
# )

enumerate <- enumerate + 4

