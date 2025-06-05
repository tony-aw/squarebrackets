
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- TRUE
test_PassByReference <- FALSE



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    i2_wo(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



temp.fun <- function(x) {
  expect_equal(
    ss2_wo(x),
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
      i2_wo(x, i = elements[[i]]),
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

x <- as.list(1:10)
expect_equal(
  i2_wo(x, 1:9, red = TRUE),
  x[[10]]
)
expect_equal(
  i2_wo(x, c(FALSE, rep(TRUE, 9)), red = TRUE),
  x[[1]]
)
enumerate <- enumerate + 2




# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(squarebrackets:::.abind(n(x, x, x), along = dim))
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
f_out.matrix <- ss2_wo
f_out.2d <- ss2_wo
f_expect.1d <- function(x, i) {
  i <- indx_wo(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}
f_out.1d <- ss2_wo


pre_subset_1d <- function(x, i) {
  return(indx_wo(i, x, names(x), length(x)))
}


sb_test <- ss2_wo.default

f_expect.arbitrary <- function(x, i, j, l) {
  i <- indx_wo(i, x, rownames(x), nrow(x))
  j <- indx_wo(j, x, colnames(x), ncol(x))
  l <- indx_wo(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test arbitrary dimensions ====


x <- array(as.list(seq_len(10^4)), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

s <- list(letters, 1:9, 1:9, 2:10)
d <- 1:4
expect_equal(
  ss2_wo(x, s, d, red = TRUE),
  x[[10, 10, 10, 1]]
)

enumerate <- enumerate + 1



# test datasets ====

pre_subset_df <- ss2_wo.data.frame

f_expect.data.frame <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, names(x), ncol(x))
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  out <- collapse::ss(x, row, col)
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}

f_out.data.frame <- ss2_wo

# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())

# test errors ====
sb_test <- i2_wo
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- ss2_wo
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())


# report number of tests

print(enumerate)

