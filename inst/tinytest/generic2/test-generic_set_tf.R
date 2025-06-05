
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


ss2_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  ss2_set(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}


# test datasets ====

pre_subset_df <- ss2_x.data.frame

f_expect.data.frame <- function(x, row = NULL, col = NULL) {
  
  tf <- \(x)x[1]
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, names(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  
  row <- as.integer(row)
  col <- as.integer(col)
  
  value <- collapse::ss(x, row, col, check = FALSE)
  value <- lapply(value, tf)
  
  x <- data.table::copy(x)
  data.table::set(x, row, col, value)
  
  return(x)
}

f_out.data.frame <- function(x, s, d) {
  
  return(ss2_set2(x, s, d, tf = \(x)x[1]))
  
}


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====

sb_test <- function(x, ...) {
  x <- data.table::copy(x)
  x2 <- x
  ss2_set(x, ..., tf = \(x)x[1])
  expect_equal(x, x2) |> errorfun()
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


if(requireNamespace("tidytable")) {
  xlist <- list(
    dt = data.table::data.table(a = 1:26, b = letters),
    tt = tidytable::tidytable(a = 1:26, b = letters)
  )
} else {
  xlist <- list(
    dt = data.table::data.table(a = 1:26, b = letters)
  )
}


# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously

print(enumerate)

