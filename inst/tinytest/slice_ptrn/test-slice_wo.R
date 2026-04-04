
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from, to, pattern) {
  ind <- (from:to)[pattern]
  ind <- seq_along(x)[-ind]
  return(x[ind])
}

slicetest <- function(x, from, to, pattern, ...) {
  stride <- stride_ptrn(from, to, pattern)
  long_x(x, stride, ..., use = -1)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_x.R"), envir = environment())


