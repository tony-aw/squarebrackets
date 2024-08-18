
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
.sub2ind_general <- squarebrackets:::.sub2ind_general
temp.fun <- function(x, lst) {
  squarebrackets:::.arr_x(x, lst, sys.call())
}

# NOTE:
# sb_set.array uses Rcpp code generated from the same string as the Rcpp code for sub2ind.
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


for(iSample in 1:10) {
  for(iDim in 2:7) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      ind <- .sub2ind_general(sub, x.dim)
      
      expect <- temp.fun(x, sub) |> as.vector() |> as.mutable_atomic()
      
      expect_equal(
        x[ind], expect
      ) |> errorfun()
      
      enumerate <- enumerate + 2
    }
  }
}


