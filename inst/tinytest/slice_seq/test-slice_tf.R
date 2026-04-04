
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  myslice <- eval_stride(stride_seq(from, to, by), x, 1)
  start <- myslice$start
  end <- myslice$end
  sign <- ifelse(start > end, -1, 1)
  by <- myslice$step_size * sign
  ind <- seq(start, end, by)
  
  x[ind] <- tf(x[ind])
  return(x)
}

slicetest <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  x <- data.table::copy(x)
  x2 <- x
  stride <- stride_seq(from, to, by)
  long_set(x, stride, tf = tf)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

subset_fun <- function(x, from, to, by, ...) {
  stride <- stride_seq(from, to, by)
  return(long_x(x, stride, ...))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_rp.R"), envir = environment())
