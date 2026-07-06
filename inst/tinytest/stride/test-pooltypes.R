# set-up ====
enumerate <- 0L


# pool is list of 1 NULL element ====
# all match
x <- rep(1L, 100L) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L

# none match
x <- rep(1L, 100L) |> as.mutatomic()
v <- 2L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L

# first match
x <- c(1L, rep(2L, 100L)) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L

# last match
x <- c(rep(2L, 100L), 1L) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L

# first and last match
x <- c(1L, rep(2L, 100L), 1L) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L



# pool is list of 1 raw vector ====

x <- sample(1:10) |> as.mutatomic()
if(all(x == 1:10)) {
  x <- sample(1:10) |> as.mutatomic()
}
stride <- stride_v(x, v = c(-Inf, 5))
first <- range(which(x <= 5))[1]
last <- range(which(x <= 5))[2]
expected <- list(as.raw(x[first:last] <= 5))
expect_equal(
  stride$pool,
  expected
)
expect_equal(stride$prepvector[5], 0L)
expect_equal(long_x(x, stride), x[x <= 5L])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 <= 5L, rp = -1L)
)
enumerate <- enumerate + 4L


# numeric vector of rare values ====
x <- c(2:4e4, sample(1:100, 4e4, TRUE), 2:4e4) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, which(x == v) - 1L)
expect_equal(stride$prepvector[5L], 1L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L


# numeric vector of common values ====

temp <- c(rep(1L, 100L), 0L)
x <- c(rep(temp, ceiling(5e4/length(temp))), 2:1e3, rep(temp, ceiling(5e4/length(temp))))
x <- as.mutatomic(x)
v <- 1.0
stride <- stride_v(x, v = v)
expect_equal(stride$pool, which(x != v) - 1L) # check for NOT equal
expect_equal(stride$prepvector[5L], -1L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L


# list of multiple types (value only present at edges) ====
n <- 1e5
x <- c(sample(1:2, n, TRUE),  sample(3:5, 2*n, TRUE), sample(1:2, n, TRUE), rep(1L, n))
x <- as.mutatomic(x)
v <- 1L

stride <- stride_v(x, v = v)

expect_equal(stride$prepvector[5L], 0L)
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L


