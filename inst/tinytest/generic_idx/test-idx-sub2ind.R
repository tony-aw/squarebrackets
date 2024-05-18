
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
temp.fun <- function(x, ...) {
  return(x[...])
}

# 5D array ====
dims <- rep(10, 5)
len <- prod(dims)

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 4, FALSE)
  ind2 <- sample(1:10, 4, FALSE)
  ind3 <- seq_len(dim(x)[3])
  ind4 <- sample(1:10, 4, FALSE)
  ind5 <- sample(1:10, 4, FALSE)
  subs <- list(ind1, ind2, ind4, ind5)
  ind <- idx(x, idx = subs, dims = c(1, 2, 4, 5))
  
  expect_equal(
    x[ind], as.vector(x[ind1, ind2, ind3, ind4, ind5])
  ) |> errorfun()
}
enumerate <- enumerate + 1



# 4D array ====
dims <- rep(10, 4)
len <- prod(dims)

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 4, FALSE)
  ind2 <- sample(1:10, 4, FALSE)
  ind3 <- seq_len(dim(x)[3])
  ind4 <- sample(1:10, 4, FALSE)
  subs <- list(ind1, ind2, ind4)
  ind <- idx(x, idx = subs, dims = c(1, 2, 4))
  
  expect_equal(
    x[ind], as.vector(x[ind1, ind2, ind3, ind4])
  ) |> errorfun()
}
enumerate <- enumerate + 1


# 3D array ====
dims <- rep(10, 3)
len <- prod(dims)

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 4, FALSE)
  ind2 <- seq_len(dim(x)[2])
  ind3 <- sample(1:10, 4, FALSE)
  subs <- list(ind1, ind3)
  ind <- idx(x, subs, dims = c(1, 3))
  
  expect_equal(
    x[ind], as.vector(x[ind1, ind2, ind3])
  ) |> errorfun()
}
enumerate <- enumerate + 1


# 2D array ====
dims <- rep(10, 2)
len <- prod(dims)

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 4, FALSE)
  ind2 <- sample(1:10, 4, FALSE)
  subs <- list(ind1, ind2)
  ind <- idx(x, subs, dims = c(1,2))
  
  expect_equal(
    x[ind], as.vector(x[ind1, ind2])
  ) |> errorfun()
}
enumerate <- enumerate + 1



# 1D array ====
dims <- rep(10, 1)
len <- prod(dims)

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 4, FALSE)
  subs <- list(ind1)
  ind <- sub2ind(subs, dims)
  
  expect_equal(
    as.vector(x[ind]), as.vector(x[ind1])
  ) |> errorfun()
}
enumerate <- enumerate + 1


# error checks ====

expect_error(
  sub2ind(list(), integer(0)),
  pattern = "`length(x.dim) == 0`",
  fixed = TRUE
)
expect_error(
  sub2ind(list(ind1), integer(2)),
  pattern = "`length(sub) != length(x.dim)`",
  fixed = TRUE
)
enumerate <- enumerate + 2

