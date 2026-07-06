
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_v2ind <- squarebrackets:::.test_v2ind

basetest_single <- function(x, ..., y, v, use, na) {
  
  ind <- .test_v2ind(1L, y, v, na, use)
  out <- x[ind]
  return(out)
  
}



basetest_numrng <- function(x, ..., y, v, use, na) {
  
  ind <- .test_v2ind(2L, y, v, na, use)
  out <- x[ind]
  return(out)
  
}


basetest_str <- function(x, ..., y, v, use, na) {
  
  ind <- .test_v2ind(3L, y, v, na, use)
  out <- x[ind]
  return(out)
  
}


slicetest <- function(x, ..., y, v, use, na) {
  stride <- stride_v(y, v = v, na = na, use = use)
  return(long_x(x, stride))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


