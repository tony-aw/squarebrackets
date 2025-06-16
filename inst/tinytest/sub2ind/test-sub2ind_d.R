
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
temp.fun <- function(x, lst) {
  squarebrackets:::.arr_x(x, lst, sys.call())
}

# NOTE:
# ss_set.default uses Rcpp code generated from the same string as the Rcpp code for sub2ind.
# Thus these tests also function as tests for arrays.

generate_data <- function(x.len) {
  list(
    sample(c(TRUE, FALSE, NA), x.len, TRUE),
    as.integer(sample(c(1:x.len - 1, NA))),
    sample(c(rnorm(x.len), NA, NaN, Inf, -Inf), x.len),
    sample(c(stringi::stri_rand_strings(x.len, 26), NA)),
    as.complex(sample(c(rnorm(x.len - 1), NA))),
    as.raw(sample(1:100, x.len, TRUE))
  )
}


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- array(x.data[[iType]], x.dim)
      sub <- lapply(x.dim, \(x) as.integer(sample(1:x, max(c(1, x)), FALSE)))
      dims <- 1:length(x.dim)
      ind <- squarebrackets:::.sub2ind_d32(sub, x.dim)
      
      expected[[i]] <- temp.fun(x, sub) |> as.vector()
      out[[i]] <- x[ind]
      
      enumerate <- enumerate + 1
      i <- i + 1
    }
  }
}

expect_equal(expected, out)



expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- array(x.data[[iType]], x.dim)
      sub <- lapply(x.dim, \(x) as.integer(sample(1:x, max(c(1, x)), FALSE)))
      dims <- 1:length(x.dim)
      
      ind <- squarebrackets:::.sub2ind_d64(sub, x.dim)
      
      expected[[i]] <- temp.fun(x, sub) |> as.vector()
      out[[i]] <- x[ind]
      
      enumerate <- enumerate + 1
      i <- i + 1
    }
  }
}

expect_equal(expected, out)




# error checks ====

expect_error(
  sub2ind(list(), integer(0)),
  pattern = "`length(x.dim) == 0`",
  fixed = TRUE
)
ind1 <- sample(1:10, 4, FALSE)
expect_error(
  sub2ind(list(ind1), integer(2)),
  pattern = "`length(sub) != length(x.dim)`",
  fixed = TRUE
)
enumerate <- enumerate + 2

