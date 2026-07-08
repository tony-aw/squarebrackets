# set-up ====
enumerate <- 0L


# pool is list of 1 NULL element ====
# all match
x <- rep(1L, 100L) |> as.mutatomic()
v <- 1L
stride <- stride_v(x, v = v)
expect_equal(stride$pool, list(NULL))
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
expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L



# pool is list of 1 bit vector ====

x <- sample(1:10) |> as.mutatomic()
if(all(x == 1:10)) {
  x <- sample(1:10) |> as.mutatomic()
}
w <- which(x <= 5)
stride <- stride_v(x, v = c(-Inf, 5))
first <- range(w)[1]
last <- range(w)[2]
rnglen <- last - first + 1L
expected <- x[first:last] <= 5
expect_equal(
  as.logical(intToBits(stride$pool[[1L]]))[1:rnglen],
  expected
)
expect_equal(long_x(x, stride), x[x <= 5L])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 <= 5L, rp = -1L)
)
enumerate <- enumerate + 4L


# list of multiple types (value only present at edges) ====
n <- 1e5
x <- c(sample(1:2, n, TRUE),  sample(3:5, 2*n, TRUE), sample(1:2, n, TRUE), rep(1L, n))
x <- as.mutatomic(x)
v <- 1L

stride <- stride_v(x, v = v)

expect_equal(long_x(x, stride), x[x == v])
x2 <- data.table::copy(x)
long_set(x, stride, rp = -1L)
expect_equal(
  x, ii_mod(x2, x2 == v, rp = -1L)
)
enumerate <- enumerate + 4L


