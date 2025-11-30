
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

test_allow_duplicates <- TRUE
test_use_factors <- TRUE
test_PassByReference <- FALSE



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    ii_x(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())


temp.fun <- function(x) {
  expect_equal(
    ss_x(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




# test elements ====

test_sb <- function(x, i) {
  i <- indx_x(i, x, names(x), length(x))
  return(x[i])
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      ii_x(x, i = elements[[i]]),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


test_sb <- function(x, i) {
  i <- indx_wo(i, x, names(x), length(x))
  return(x[i])
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      ii_x(x, elements[[i]], -1),
      test_sb(x, elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())




# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(squarebrackets:::.abind(n(x, x, x), along = dim))
}


pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  return(x[row, col])
}



f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col, drop = FALSE])
}
f_out.matrix <- ss_x
f_out.2d <- ss_x
f_expect.1d <- function(x, i) {
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}
f_out.1d <- ss_x


pre_subset_1d <- function(x, i) {
  return(indx_x(i, x, names(x), length(x)))
}

sb_test <- ss_x

f_expect.arbitrary <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())





# test datasets ====


pre_subset_df <- sbt_x

f_expect.data.frame <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, names(x), ncol(x))
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  out <- collapse::ss(x, row, col)
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}

f_out.data.frame <- sbt_x


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")


sys.source(file.path(getwd(), "source", "sourcetest-obsvars.R"), envir = environment())


# test errors ====
sb_test <- ii_x
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- ss_x
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())



# report number of tests

print(enumerate)

