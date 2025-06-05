
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


i_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  i_set(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}
ss_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  ss_set.default(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  tempfun <- function(x) {
    x <- as.mutatomic(x)
    x[] <- x[1]
    return(x)
  }
  expect_equal(
    i_set2(x, tf = \(x)x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



temp.fun <- function(x) {
  tempfun <- function(x) {
    x <- as.mutatomic(x)
    x[] <- x[1]
    return(x)
  }
  expect_equal(
    ss_set2(x, tf = \(x)x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




# test elements ====

test_sb <- function(x, i) {
  if(is.atomic(x)) x <- as.mutatomic(x)
  i <- indx_x(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- min(x[i])
  return(x)
}


temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      i_set2(x, i = elements[[i]], tf = min),
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
  
  if(is.atomic(x)) x <- as.mutatomic(x)
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  tf <- mean
  
  x[row, col] <- tf(x[row, col])
  
  return(x)
}


f_out.2d <- function(x, s, d) {
  
  return(ss_set2(x, s, d, tf = mean))
}


pre_subset_1d <- function(x, i) {
  return(indx_x(i, x, names(x), length(x)))
}

f_expect.1d <- function(x, i) {
  
  if(is.atomic(x)) x <- as.mutatomic(x)
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  tf <- mean
  
  x[i] <- tf(x[i])
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  return(ss_set2(x, s, d, tf = mean))
}


sb_test <- function(x, ...) {
  return(ss_set2(x, ..., tf = mean))
}

f_expect.arbitrary <- function(x, i, j, l) {
  if(is.atomic(x)) x <- as.mutatomic(x)
  tf <- mean
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  tf <- mean
  x[i, j, , l] <- tf(x[i, j, , l])
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# test errors ====


sb_test <- function(x, ...) {
  x <- as.mutatomic(x)
  i_set(x, ..., tf = \(x)x[1])
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())


sb_test <- function(x, ...) {
  x <- as.mutatomic(x)
  ss_set(x, ..., tf = \(x)x[1])
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())



x <- as.mutatomic(x)
expect_error(
  i_set(x, i = 1, tf = "foo"),
  pattern = "`tf` must be a function"
)

enumerate <- enumerate + 1

# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously
print(enumerate)

