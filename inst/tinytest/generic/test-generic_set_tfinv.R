
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb_set(x, ..., inv = TRUE)
  expect_equal(x, x2) |> errorfun()
  return(x)
}
sb_set2.array <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb_set.array(x, ..., inv = TRUE)
  expect_equal(x, x2) |> errorfun()
  return(x)
}

# test elements ====

test_sb <- function(x, i) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_rm(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- min(x[i])
  return(x)
}


temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb_set2(x, i = elements[[i]], tf = min),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test matrix & array ====

rep3.bind <- function(x, dim) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  return(abind::abind(x, x, x, along = dim))
}

subset_mat <- function(x, row = NULL, col = NULL) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  
  tf <- mean
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col] <- tf(x[row, col])
  
  return(x)
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      expect_equal(
        sb_set2(x, row = row[[i]], col = col[[j]], tf = mean),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb_set2(x, row = row[[i]], col = col[[j]], tf = mean) |>
                    is.matrix()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


subset_1d <- function(x, i) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  tf <- mean
  i <- indx_rm(i, x, dimnames(x)[[1]], length(x))
  x[i] <- tf(x[i])
  return(x)
}

temp.fun.1d <- function(x, row) {
  for(i in 1:length(row)) {
    expect_equal(
      sb_set2(x, row[[i]], 1, tf = mean),
      subset_1d(x, row[[i]])
    ) |> errorfun()
    expect_true(sb_set2(x, row[[i]], 1, tf = mean) |>
                  is.array()) |> errorfun()
    assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
  }
}

temp.fun.2d <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      sub <- n(row[[i]], col[[j]])
      dims <- 1:2
      rem <- which(vapply(sub, is.null, logical(1L)))
      if(length(rem) > 0L) {
        sub <- sub[-rem]
        dims <- dims[-rem]
      }
      
      expect_equal(
        sb_set2.array(x, sub, dims, tf = mean),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb_set2.array(x, sub, dims, tf = mean) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- function(...) sb_set2.array(..., tf = mean)

temp.fun.arbitrary <- function(x, i, j, l) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  tf <- mean
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- tf(x[i, j, , l])
  return(x)
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


x <- as.mutable_atomic(array(seq_len(10^4), dim = c(10, 10, 10, 10)))
rownames(x) <- c(letters[1:8], "a", NA)
tf <- function(x) -x

sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, sub, dims, tf = tf),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], tf)
)

sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, sub, dims, tf = tf),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], tf)
)

sub <- list(c("a"), c(1:4), rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, sub, dims, tf = tf),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], tf)
)

enumerate <- enumerate + 3


# test errors ====
sb_test <- function(x, ...) {
  x <- data.table::copy(x)
  sb_set(x, ..., tf = \(x)x[1])
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())

x <- as.mutable_atomic(x)
expect_error(
  sb_set(x, i = 1, tf = "foo"),
  pattern = "`tf` must be a function"
)

enumerate <- enumerate + 1


# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously

print(enumerate)

