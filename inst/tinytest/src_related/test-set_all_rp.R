
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, rp) {
  x <- data.table::copy(x)
  i_set(x, rp = rp)
  return(x)
}
tempfun1 <- function(x, rp) {
  x[] <- rp
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


# array ====

expected <- out <- list()
i <- 1

for(iSample in 1:10) {
  for(iDim in 2:9) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- mutatomic::as.mutatomic(array(x.data[[iType]], x.dim))
      
      expected[[i]] <- tempfun1(x, rp.lst[[iType]])
      out[[i]] <- tempfun2(x, rp.lst[[iType]])
      
      x2 <- x
      i_set(x, rp = rp.lst[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      enumerate <- enumerate + 2
      i <- i + 1
    }
  }
}
expect_equal(expected, out)


# matrix ====
n <- 5

expected <- out <- list()
i <- 1

x.dim <- rep(n, 2)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- mutatomic::as.mutatomic(array(x.data[[j]], x.dim))
    
    expected[[i]] <- tempfun1(x, rp.lst[[j]])
    out[[i]] <- tempfun2(x, rp.lst[[j]])
    
    x2 <- x
    i_set(x, rp = rp.lst[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    enumerate <- enumerate + 2
    i <- i + 1
  }
  
}
expect_equal(expected, out)


# vector ====
x.len <- 100

expected <- out <- list()
i <- 1

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- mutatomic::as.mutatomic(x.data[[j]])
    
    expected[[i]] <- tempfun1(x, rp.lst[[j]])
    out[[i]] <- tempfun2(x, rp.lst[[j]])
    
    x <- data.table::copy(x)
    x2 <- x
    i_set(x, rp = rp.lst[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
    enumerate <- enumerate + 2
    i <- i + 1
  }
  
}
expect_equal(expected, out)



