
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

test_allow_duplicates <- TRUE
test_use_factors <- TRUE
test_PassByReference <- FALSE



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    ii2_x(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())


temp.fun <- function(x) {
  expect_equal(
    ss2_x(x),
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
      ii2_x(x, i = elements[[i]]),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


indx_general <- list(
  logical(0),
  1, 1:2, 2:1, c(1, 1, 1), 
  c(rep(TRUE, 24), rep(FALSE, 24)),
  rep(TRUE, 48), rep(FALSE, 48),
  function(x) x>5
)

indx_named <- c(indx_general, "ab", list(c("ab", "ab")))

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())

x <- as.list(1:10)
expect_equal(
  ii2_x(x, 1, red = TRUE),
  x[[1]]
)
expect_equal(
  ii2_x(x, c(TRUE, rep(FALSE, 9)), red = TRUE),
  x[[1]]
)
enumerate <- enumerate + 2



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
f_out.matrix <- ss2_x
f_out.2d <- ss2_x
f_expect.1d <- function(x, i) {
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}
f_out.1d <- ss2_x


pre_subset_1d <- function(x, i) {
  return(indx_x(i, x, names(x), length(x)))
}

sb_test <- ss2_x

f_expect.arbitrary <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())




# test arbitrary dimensions ====

subset_arr <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l, drop = FALSE]
}

x <- array(as.list(seq_len(10^4)), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

s <- list("b", 1, 1, 2)
d <- 1:4
expect_equal(
  ss2_x(x, s, d, red = TRUE),
  x[["b", 1, 1, 2]]
)

enumerate <- enumerate + 1


# test datasets ====


pre_subset_df <- ss2_x.data.frame

f_expect.data.frame <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, names(x), ncol(x))
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  out <- collapse::ss(x, row, col)
  names(out) <- make.names(names(out), unique = TRUE)
  return(out)
}

f_out.data.frame <- ss2_x


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")


sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())

# test errors ====
sb_test <- ii2_x
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())

sb_test <- ss2_x
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())



# report number of tests

print(enumerate)

