
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

.test_v2ind <- squarebrackets:::.test_v2ind


basetest_single <- function(x, ..., y, v, na, use) {
  
  tf <- parent.frame()$tf
  
  ind <- .test_v2ind(1L, p, v, na, use)
  
  x[ind] <- tf(x[ind])
  return(x)
}



basetest_numrng <- function(x, ..., y, v, na, use) {
  
  tf <- parent.frame()$tf
  
  ind <- .test_v2ind(2L, p, v, na, use)
  
  x[ind] <- tf(x[ind])
  return(x)
}


basetest_str <- function(x, ..., y, v, na, use) {
  
  tf <- parent.frame()$tf
  
  ind <- .test_v2ind(3L, p, v, na, use)
  
  x[ind] <- tf(x[ind])
  return(x)
}


slicetest <- function(x, ..., y, v, na, use) {
  
  x <- data.table::copy(x)
  x2 <- x
  
  tf <- parent.frame()$tf
  
  stride <- stride_pv(p, v, na)
  long_set(x, stride, use, tf = tf)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


