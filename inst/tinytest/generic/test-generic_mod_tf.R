
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
    ii_mod(x, tf = \(x)x[1]),
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
    ss_mod(x, tf = \(x)x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i) {
  i <- indx_x(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- min(x[i])
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      ii_mod(x, i = elements[[i]], tf = min),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())



test_sb <- function(x, i) {
  i <- indx_wo(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- min(x[i])
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      ii_mod(x, elements[[i]], -1, tf = min),
      test_sb(x, elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())



# test array ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}

pre_subset_1d <- function(x, i) {
  return(indx_x(i, x, names(x), length(x)))
}

f_expect.1d <- function(x, i) {
  
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  tf <- mean
  
  x[i] <- tf(x[i])
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  return(ss_mod(x, s, d, tf = mean))
}


sb_test <- function(x, ...) {
  return(ss_mod.default(x, ..., tf = mean))
}

f_expect.arbitrary <- function(x, i, j, l) {
  tf <- mean
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- tf(x[i, j, , l])
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# row,col ===

pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  sbt_x(x, row, col)
}


f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  
  tf <- mean
  i <- indx_x(row, x, rownames(x), nrow(x))
  j <- indx_x(col, x, colnames(x), ncol(x))
  x[i, j] <- tf(x[i, j])
  return(x)
}

f_out.2d <- f_out.matrix <- function(x, row, col) {
  
  return(sbt_mod(x, row = row, col = col, tf = mean))
}


sys.source(file.path(getwd(), "source", "sourcetest-rowcol.R"), envir = environment())



# test errors ====
sb_test <- function(...)ii_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- function(...)ss_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


expect_error(
  ii_mod(1:10, i = 1, tf = "foo"),
  pattern = "`tf` must be a function"
)

# report number of tests

print(enumerate)

