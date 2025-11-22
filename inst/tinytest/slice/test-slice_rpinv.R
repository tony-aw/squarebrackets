
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  ind <- seq_along(x)[-seq(start, end, by)]
  
  rp <- parent.frame()$rp
  
  x[ind] <- rp
  return(x)
}

slicetest <- function(x, from = NULL, to = NULL, by = 1L, tf) {
  x <- data.table::copy(x)
  x2 <- x
  
  rp <- parent.frame()$rp
  
  slice_set(x, from, to, by, inv = TRUE, rp = rp)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

subset_fun <- slice_wo

sys.source(file.path(getwd(), "source", "sourcetest-elements_rp.R"), envir = environment())

