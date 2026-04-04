
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from, to, pattern, tf) {
  ind <- (from:to)[pattern]
  ind <- seq_along(x)[-ind]
  
  x[ind] <- tf(x[ind])
  return(x)
}

slicetest <- function(x, from, to, pattern, tf) {
  x <- data.table::copy(x)
  x2 <- x
  stride <- stride_ptrn(from, to, pattern)
  long_set(x, stride, use = -1, tf = tf)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

subset_fun <- function(x, from, to, pattern, ...) {
  stride <- stride_ptrn(from, to, pattern)
  return(long_x(x, stride, use = -1, ...))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_rp.R"), envir = environment())
