
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0


tempfun2 <- function(x, sub, dims, rp) {
  ind <- idx.array(x, sub, dims)
  x[ind] <- rp
  return(x)
}

rp.lst <- list(
  NA,
  -1000L,
  -Inf,
  "NA; NaN; Inf; -Inf",
  as.complex(-1000),
  as.raw(0)
)

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


# rcpp_set_array_general ====

tempfun1 <- function(x, sub, rp) {
  x <- data.table::copy(x)
  squarebrackets:::.rcpp_set_array_general_atomic(x, sub, dim(x), rp)
  return(x)
}

expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:8) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      out[[i]] <- tempfun1(x, sub, rp.lst[[iType]])
      expected[[i]] <- tempfun2(x, sub, dims, rp.lst[[iType]])
     
      
      x <- data.table::copy(x)
      x2 <- x
      squarebrackets:::.rcpp_set_array_general_atomic(x, sub, dim(x), rp.lst[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:8) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      rp <- rep(rp.lst[[iType]], prod(lengths(sub)))
      
      out[[i]] <- tempfun1(x, sub, rp)
      expected[[i]] <- tempfun2(x, sub, dims, rp)
      
      
      x <- data.table::copy(x)
      x2 <- x
      squarebrackets:::.rcpp_set_array_general_atomic(x, sub, dim(x), rp)
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)




# rcpp_set_array_16d ====

tempfun1 <- function(x, sub, rp) {
  x <- data.table::copy(x)
  squarebrackets:::.rcpp_set_array_16d(x, rp, sub, dim(x))
  return(x)
}


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:8) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      out[[i]] <- tempfun1(x, sub, rp.lst[[iType]])
      expected[[i]] <- tempfun2(x, sub, dims, rp.lst[[iType]])
      
      
      x <- data.table::copy(x)
      x2 <- x
      squarebrackets:::.rcpp_set_array_16d(x, rp.lst[[iType]], sub, dim(x))
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)


tempfun1 <- function(x, sub, rp) {
  x <- data.table::copy(x)
  squarebrackets:::.rcpp_set_array_16d(x, rp, sub, dim(x))
  return(x)
}


expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:8) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      sub <- lapply(x.dim, \(x) sample(1:x, max(c(1, x)), FALSE))
      dims <- 1:length(x.dim)
      
      rp <- rep(rp.lst[[iType]], prod(lengths(sub)))
      
      out[[i]] <- tempfun1(x, sub, rp)
      expected[[i]] <- tempfun2(x, sub, dims, rp)
      
      
      x <- data.table::copy(x)
      x2 <- x
      squarebrackets:::.rcpp_set_array_16d(x, rp, sub, dim(x))
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)

