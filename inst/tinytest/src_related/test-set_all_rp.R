
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0
tempfun2 <- function(x, rp) {
  x <- data.table::copy(x)
  sb_set.array(x, rp = rp)
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

for(iSample in 1:10) {
  for(iDim in 2:7) {
    x.dim <- sample(1:6, size = iDim, replace = TRUE)
    x.len <- prod(x.dim)
    x.data <- generate_data(x.len)
    for(iType in seq_along(x.data)) {
      x <- as.mutable_atomic(array(x.data[[iType]], x.dim))
      
      expect_equal(
        tempfun1(x, rp.lst[[iType]]), tempfun2(x, rp.lst[[iType]])
      ) |> errorfun() # test indexing & atomic type recognition
      
      x2 <- x
      sb_set(x, rp = rp.lst[[iType]])
      expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
      enumerate <- enumerate + 2
    }
  }
}


# matrix ====
n <- 5

x.dim <- rep(n, 2)
x.len <- prod(x.dim)

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(array(x.data[[j]], x.dim))
    
    expect_equal(
      tempfun1(x, rp.lst[[j]]), tempfun2(x, rp.lst[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set(x, rp = rp.lst[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
    
    enumerate <- enumerate + 2
  }
  
}


# vector ====
x.len <- 100

for(i in 1:10) {
  x.data <- generate_data(x.len)
  
  for(j in 1:length(x.data)) {
    x <- as.mutable_atomic(x.data[[j]])
    
    expect_equal(
      tempfun1(x, rp.lst[[j]]), tempfun2(x, rp.lst[[j]])
    ) |> errorfun() # test indexing & atomic type recognition
    
    x2 <- x
    sb_set(x, rp = rp.lst[[j]])
    expect_equal(x,x2) |> errorfun() # test indexing & pass-by-reference
    
    enumerate <- enumerate + 2
  }
  
}



