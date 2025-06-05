
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
temp.fun <- function(x, lst) {
  squarebrackets:::.arr_x(x, lst, sys.call())
}

# NOTE:
# ss_set.default uses Rcpp code generated from the same string as the Rcpp code for sub2ind.
# Thus these tests also function as tests for arrays.


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x <- array(sample(1:100), dim = x.dim)
    sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
    
    ind <- squarebrackets:::.sub2ind_chunk32(sub, x.dim)
    
    expected[[i]] <- temp.fun(x, sub) |> as.vector()
    out[[i]] <- x[ind]
    
    enumerate <- enumerate + 1
    i <- i + 1
  }
}
expect_equal(expected, out)


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x <- array(rnorm(100), dim = x.dim)
    sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
    
    ind <- squarebrackets:::.sub2ind_chunk64(sub, x.dim)
    
    expected[[i]] <- temp.fun(x, sub) |> as.vector()
    out[[i]] <- x[ind]
    
    enumerate <- enumerate + 1
    i <- i + 1
  }
}
expect_equal(expected, out)



x.dim <- rep(c(1, 3), 8)
x.len <- prod(x.dim)
x <- array(sample(1:100), dim = x.dim)
sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
ind <- squarebrackets:::.sub2ind_chunk32(sub, x.dim)
expected <- temp.fun(x, sub) |> as.vector()
out <- x[ind]
expect_equal(
  expected, out
)

x.dim <- x.dim <- rep(c(1, 3), 8)
x.len <- prod(x.dim)
x <- array(rnorm(100), dim = x.dim)
sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
ind <- squarebrackets:::.sub2ind_chunk64(sub, x.dim)
expected <- temp.fun(x, sub) |> as.vector()
out <- x[ind]
expect_equal(
  expected, out
)

enumerate <- enumerate + 2

