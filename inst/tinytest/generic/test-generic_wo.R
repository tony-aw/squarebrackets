
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- TRUE
test_PassByReference <- FALSE


# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    i_wo(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())

x <- factor(letters)
temp.fun(x)


temp.fun <- function(x) {
  expect_equal(
    ss_wo(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




# test elements ====


test_sb <- function(x, i) {
  i <- indx_wo(i, x, names(x), length(x))
  return(x[i])
}


temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      i_wo(x, i = elements[[i]]),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}


pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  return(x[row, col])
}

f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col, drop = FALSE])
}
f_out.matrix <- ss_wo
f_out.2d <- ss_wo.default
f_expect.1d <- function(x, i) {
  i <- indx_wo(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}
f_out.1d <- ss_wo


pre_subset_1d <- function(x, i) {
  return(indx_wo(i, x, names(x), length(x)))
}


sb_test <- ss_wo.default

f_expect.arbitrary <- function(x, i, j, l) {
  i <- indx_wo(i, x, rownames(x), nrow(x))
  j <- indx_wo(j, x, colnames(x), ncol(x))
  l <- indx_wo(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())




# test errors ====
sb_test <- i_wo
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())


sb_test <- ss_wo
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


# report number of tests

print(enumerate)

