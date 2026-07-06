
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from, to, by = 1L, tf) {
  myslice <- eval_stride(stride_seq(from, to, by), x)
  start <- myslice$start
  end <- myslice$end
  sign <- ifelse(start > end, -1, 1)
  by <- myslice$step_size * sign
  ind <- seq_along(x)[-seq(start, end, by)] # inverting here
  
  rp <- parent.frame()$rp
  
  x[ind] <- rp
  return(x)
}

slicetest <- function(x, from, to, by = 1L, tf) {
  x <- data.table::copy(x)
  x2 <- x
  
  rp <- parent.frame()$rp
  
  stride <- stride_seq(from, to, by, -1)
  long_set(x, stride, rp = rp)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

subset_fun <- function(x, from, to, by, ...) {
  stride <- stride_seq(from, to, by, -1)
  return(long_x(x, stride, ...))
}

sys.source(file.path(getwd(), "source", "sourcetest-elements_rp.R"), envir = environment())
