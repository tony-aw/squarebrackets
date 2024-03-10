
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

# as.mutable_atomic vs mutable_atomic ====
x <- 1:10
names(x) <- letters[1:10]
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:10, names = letters[1:10])
)

x <- matrix(1:20, ncol = 4)
colnames(x) <- letters[1:4]
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:20, dim = c(5, 4), dimnames = n(NULL, letters[1:4]))
)

x <- array(1:27, dim = c(3,3,3))
dimnames(x) <- n(NULL, NULL, letters[1:3])
expect_equal(
  as.mutable_atomic(x),
  mutable_atomic(1:27, dim = c(3,3,3), dimnames = n(NULL, NULL, letters[1:3]))
)


# partial matrix sub-setting ====
x <- as.mutable_atomic(matrix(1:20, ncol = 4))
expect_equal(
  sb_x(x, row = 1),
  x[1, ,drop = FALSE]
)
expect_equal(
  sb_x(x, col = 1),
  x[, 1, drop = FALSE]
)

expect_equal(
  sb_rm(x, row = 1),
  x[-1, ,drop = FALSE]
)
expect_equal(
  sb_rm(x, col = 1),
  x[, -1, drop = FALSE]
)
enumerate <- enumerate + 4


# partial array3d sub-setting ====
x <- as.mutable_atomic(array(1:27, dim = c(3,3,3)))
expect_equal(
  sb_x(x, rcl = n(1, NULL, NULL)),
  x[1, , ,drop = FALSE]
)
expect_equal(
  sb_x(x, rcl = n(NULL, 1, NULL)),
  x[, 1, , drop = FALSE]
)
expect_equal(
  sb_x(x, rcl = n(NULL, NULL, 1)),
  x[, , 1 , drop = FALSE]
)

expect_equal(
  sb_rm(x, rcl = n(1, NULL, NULL)),
  x[-1, , ,drop = FALSE]
)
expect_equal(
  sb_rm(x, rcl = n(NULL, 1, NULL)),
  x[, -1, , drop = FALSE]
)
expect_equal(
  sb_rm(x, rcl = n(NULL, NULL, 1)),
  x[, , -1 , drop = FALSE]
)

enumerate <- enumerate + 6


# partial (n=4)-dim array sub-setting ====
x <- as.mutable_atomic(array(1:81, dim = c(3,3,3, 3)))
expect_equal(
  sb_x(x, idx = n(1), dims = 1),
  x[1, , ,,drop = FALSE]
)
expect_equal(
  sb_x(x, idx = n(1), dims = 2),
  x[, 1, ,, drop = FALSE]
)
expect_equal(
  sb_x(x, idx = n(1), dims = 3),
  x[, , 1 ,, drop = FALSE]
)
expect_equal(
  sb_x(x, idx = n(1), dims = 4),
  x[, , ,1, drop = FALSE]
)

expect_equal(
  sb_rm(x, idx = n(1), dims = 1),
  x[-1, , ,,drop = FALSE]
)
expect_equal(
  sb_rm(x, idx = n(1), dims = 2),
  x[, -1, ,, drop = FALSE]
)
expect_equal(
  sb_rm(x, idx = n(1), dims = 3),
  x[, , -1 ,, drop = FALSE]
)
expect_equal(
  sb_rm(x, idx = n(1), dims = 4),
  x[, , ,-1, drop = FALSE]
)

enumerate <- enumerate + 8


