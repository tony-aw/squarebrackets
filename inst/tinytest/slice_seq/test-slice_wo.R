
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from = NULL, to = NULL, by = 1L) {
  myslice <- eval_stride(stride_seq(from, to, by), x, 1)
  start <- myslice$start
  end <- myslice$end
  sign <- ifelse(start > end, -1, 1)
  by <- myslice$step_size * sign
  ind <- seq_along(x)[-seq(start, end, by)]
  return(x[ind])
}

slicetest <- function(x, from, to, by, ...) {
  stride <- stride_seq(from, to, by)
  long_x(x, stride, ..., use = -1)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_x.R"), envir = environment())


