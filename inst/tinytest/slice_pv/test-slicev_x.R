
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_v2ind <- squarebrackets:::.test_v2ind

basetest_single <- function(x, ..., p, v, use, na) {
  
  ind <- .test_v2ind(1L, p, v, na, use)
  out <- x[ind]
  return(out)
  
}



basetest_numrng <- function(x, ..., p, v, use, na) {
  
  ind <- .test_v2ind(2L, p, v, na, use)
  out <- x[ind]
  return(out)
  
}


basetest_str <- function(x, ..., y, v, use, na) {
  
  ind <- .test_v2ind(3L, p, v, na, use)
  out <- x[ind]
  return(out)
  
}


slicetest <- function(x, ..., p, v, use, na) {
  stride <- stride_pv(p, v, na)
  return(long_x(x, stride, use))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


