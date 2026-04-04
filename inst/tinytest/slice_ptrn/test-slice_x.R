
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from = NULL, to = NULL, ptrn) {
  ind <- (from:to)[ptrn]
  return(x[ind])
}

slicetest <- function(x, from, to, ptrn, ...) {
  stride <- stride_ptrn(from, to, ptrn)
  long_x(x, stride, ...)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_x.R"), envir = environment())

