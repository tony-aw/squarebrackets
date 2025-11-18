
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_PassByReference <- FALSE


# test missing arguments (NULL) ====

temp.fun <- function(x) {
  tempfun <- function(x) {
    x[] <- -1
    return(x)
  }
  expect_equal(
    ii_mod(x, tf = \(x) return(-1)),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



temp.fun <- function(x) {
  tempfun <- function(x) {
    x[] <- -1
    return(x)
  }
  expect_equal(
    ss_mod(x, tf = \(x) return(-1)),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i) {
  if(!is.list(x)) {
    i <- indx_x(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- min(x[i])
    return(x)
  }
  if(is.list(x)) {
    i <- indx_x(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- lapply(x[i], min)
    if(length(i) == 1) x[[i]] <- min(x[[i]])
    return(x)
  }
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


indx_general <- list(
  integer(0),
  1, 1:2, 2:1,
  c(rep(TRUE, 24), rep(FALSE, 24)),
  rep(TRUE, 48), rep(FALSE, 48),
  c(TRUE, rep(FALSE, 47)), c(FALSE, rep(TRUE, 47)),
  function(x) x>5
)

indx_named <- c(indx_general, "ab")

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
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  tf <- mean
  x[row, col] <- lapply(x[row, col], tf)
  
  return(x)
}


f_out.2d <- function(x, s, d) {
  
  return(ss_mod(x, s, d, tf = mean))
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
  x[i] <- lapply(x[i], tf)
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  return(ss_mod(x, s, d, tf = mean))
}


sb_test <- function(x, ...) {
  return(ss_mod(x, ..., tf = mean))
}

f_expect.arbitrary <- function(x, i, j, l) {
  tf <- mean
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- lapply(x[i, j, , l], tf)
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test datasets ====


pre_subset_df <- sbt_x

f_expect.data.frame <- function(x, row = NULL, col = NULL) {
  
  tf <- function(x)x[1]
  
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

f_out.data.frame <- function(x, obs = NULL, vars = NULL) {
  
  return(sbt_mod(x, obs, vars, tf = \(x)x[1]))
  
}


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")
sys.source(file.path(getwd(), "source", "sourcetest-obsvars.R"), envir = environment())


# test errors ====
sb_test <- function(...)ii_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- function(...)ss_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


expect_error(
  ii_mod(as.list(1:10), i = 1, tf = "foo"),
  pattern = "`tf` must be a function"
)

# report number of tests

print(enumerate)

