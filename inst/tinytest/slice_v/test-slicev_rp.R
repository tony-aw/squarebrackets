
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_v2ind <- squarebrackets:::.test_v2ind



basetest_single <- function(x, ..., y, v, na, use) {
  
  rp <- parent.frame()$rp
  
  ind <- .test_v2ind(1L, y, v, na, use)
  
  x[ind] <- rp
  return(x)
}



basetest_numrng <- function(x, ..., y, v, na, use) {
  
  rp <- parent.frame()$rp
  
  ind <- .test_v2ind(2L, y, v, na, use)
  
  x[ind] <- rp
  return(x)
}


basetest_str <- function(x, ..., y, v, na, use) {
  
  rp <- parent.frame()$rp
  
  ind <- .test_v2ind(3L, y, v, na, use)
  
  x[ind] <- rp
  return(x)
}


slicetest <- function(x, ..., y, v, na, use) {
  
  x <- data.table::copy(x)
  x2 <- x
  
  rp <- parent.frame()$rp
  
  stride <- stride_v(y, v = v, na = na, use = use)
  long_set(x, stride, rp = rp)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


