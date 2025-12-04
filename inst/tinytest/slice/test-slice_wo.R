
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


basetest <- function(x, from = NULL, to = NULL, by = 1L) {
  myslice <- cp_seq(x, 0L, from, to, by)
  start <- myslice$start
  end <- myslice$end
  by <- myslice$by
  ind <- seq_along(x)[-seq(start, end, by)]
  return(x[ind])
}

slicetest <- slice_wo

sys.source(file.path(getwd(), "source", "sourcetest-elements_x.R"), envir = environment())


