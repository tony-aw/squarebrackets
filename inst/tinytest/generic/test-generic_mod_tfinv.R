
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- FALSE


# test elements ====

test_sb <- function(x, i) {
  if(!is.list(x)) {
    i <- indx_rm(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- min(x[i])
    return(x)
  }
  if(is.list(x)) {
    i <- indx_rm(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- lapply(x[i], min)
    if(length(i) == 1) x[[i]] <- min(x[[i]])
    return(x)
  }
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb_mod(x, i = elements[[i]], tf = min, inv = TRUE),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test matrix & 3d array ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}

subset_mat <- function(x, row = NULL, col = NULL) {
  
  tf <- mean
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col] <- tf(x[row, col])
  
  return(x)
}



temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
        expect_equal(
          sb_mod(x, row = row[[i]], col = col[[j]], tf = mean, inv = TRUE),
          subset_mat(x, row[[i]], col[[j]])
        ) |> errorfun()
        expect_true(sb_mod(x, row = row[[i]], col = col[[j]], tf = mean, inv = TRUE) |>
                      is.matrix()) |> errorfun()
        assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}



sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test arbitrary dimensions ====

subset_arr <- function(x, i, j, l, tf) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- tf(x[i, j, , l, drop = FALSE])
  return(x)
}

x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)
tf <- function(x) -x

idx <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_mod(x, idx, dims, tf = tf, inv = TRUE),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
)

idx <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_mod(x, idx, dims, tf = tf, inv = TRUE),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
)

idx <- list(c("a"), c(1:4), rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_mod(x, idx, dims, tf = tf, inv = TRUE),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
)

enumerate <- enumerate + 3



# test errors ====
sb_test <- function(...)sb_mod(..., tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())

expect_error(
  sb_mod(1:10, i = 1, tf = "foo", inv = TRUE),
  pattern = "`tf` must be a function"
)

# report number of tests

print(enumerate)

