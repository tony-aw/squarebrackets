
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from, to, pattern, tf) {
  ind <- (from:to)[pattern]
  
  rp <- parent.frame()$rp
  
  x[ind] <- rp
  return(x)
}

slicetest <- function(x, from, to, pattern, tf) {
  x <- data.table::copy(x)
  x2 <- x
  
  rp <- parent.frame()$rp
  
  stride <- stride_ptrn(from, to, pattern)
  long_set(x, stride, rp = rp)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

subset_fun <- function(x, from, to, pattern, ...) {
  stride <- stride_ptrn(from, to, pattern)
  return(long_x(x, stride, ...))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_rp.R"), envir = environment())

