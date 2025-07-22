
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- FALSE



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  tempfun <- function(x) {
    x[] <- x[1]
    return(x)
  }
  expect_equal(
    ii_mod(x, inv = TRUE, tf = \(x)x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())


temp.fun <- function(x) {
  tempfun <- function(x) {
    x[] <- x[1]
    return(x)
  }
  expect_equal(
    ss_mod(x, inv = TRUE, tf = \(x)x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i) {
  if(!is.list(x)) {
    i <- indx_wo(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- min(x[i])
    return(x)
  }
  if(is.list(x)) {
    i <- indx_wo(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- lapply(x[i], min)
    if(length(i) == 1) x[[i]] <- min(x[[i]])
    return(x)
  }
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      ii_mod(x, i = elements[[i]], tf = min, inv = TRUE),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test matrix & array ====

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
  
  tf <- mean
  
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col] <- tf(x[row, col])
  
  return(x)
}

f_out.matrix <- function(x, row, col) {
  
  return(ss_mod(x, row = row, col = col, inv = TRUE, tf = mean))
}

f_out.2d <- function(x, s, d) {
  
  return(ss_mod.default(x, s, d, inv = TRUE, tf = mean))
}


pre_subset_1d <- function(x, i) {
  return(indx_wo(i, x, names(x), length(x)))
}

f_expect.1d <- function(x, i) {
  
  i <- indx_wo(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  tf <- mean
  
  x[i] <- tf(x[i])
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  return(ss_mod(x, s, d, inv = TRUE, tf = mean))
}


sb_test <- function(x, ...) {
  return(ss_mod.default(x, ..., inv = TRUE, tf = mean))
}

f_expect.arbitrary <- function(x, i, j, l) {
  tf <- mean
  i <- indx_wo(i, x, rownames(x), nrow(x))
  j <- indx_wo(j, x, colnames(x), ncol(x))
  l <- indx_wo(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- tf(x[i, j, , l])
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# test errors ====
sb_test <- function(...)ii_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- function(...)ss_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


expect_error(
  ii_mod(1:10, i = 1, tf = "foo", inv = TRUE),
  pattern = "`tf` must be a function"
)

# report number of tests

print(enumerate)

