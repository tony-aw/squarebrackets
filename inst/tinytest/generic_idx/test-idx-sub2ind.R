
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
temp.fun <- function(x, lst) {
  squarebrackets:::.arr_x(x, lst, sys.call())
}

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
  for(iDim in 2:8) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- array(x.data[[iType]], x.dim)
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      ind <- idx(x, sub = sub, dims = dims)
      
      expected[[i]] <- temp.fun(x, sub) |> as.vector()
      out[[i]] <- x[ind]
      
      enumerate <- enumerate + 1
      i <- i + 1
    }
  }
}

expect_equal(expected, out)



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

