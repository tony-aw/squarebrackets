
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
  sb_set(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}
sb_set2.array <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb_set.array(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}


# test missing arguments (NULL) ====

temp.fun <- function(x) {
  tempfun <- function(x) {
    x <- as.mutable_atomic(x)
    x[] <- x[1]
    return(x)
  }
  expect_equal(
    sb_set2(x, rp = x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i, rp) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_x(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- rp
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1L, length(indx_x(elements[[i]], x, names(x), length(x))))
    if(is.list(x)) rp1 <- as.list(rp1)
    if(is.list(x) && length(rep) != 1) rp2 <- as.list(rp)
    expect_equal(
      sb_set2(x, i = elements[[i]], rp = rp1),
      test_sb(x, i = elements[[i]], rp = rp2)
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
  
  rp <- parent.frame()$rp
  
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col] <- rp
  
  return(x)
}

f_out.matrix <- function(x, row, col) {
  
  rp <- parent.frame()$rp
  
  return(sb_set2(x, row = row, col = col, rp = rp))
}

f_out.2d <- function(x, s, d) {
  
  rp <- parent.frame()$rp
  
  return(sb_set2.array(x, s, d, rp = rp))
}


pre_subset_1d <- function(x, i) {
  return(indx_x(i, x, names(x), length(x)))
}

f_expect.1d <- function(x, i) {
  
  rp <- parent.frame()$rp
  
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))

  if(any_empty_indices(i)) {
    return(x)
  }

  x[i] <- rp
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  rp <- parent.frame()$rp
  
  return(sb_set2(x, s, d, rp = rp))
}


sb_test <- function(x, ...) {
  x <- as.mutable_atomic(x)
  rp <- sb_x.array(x, ...) * -1
  sb_set.array(x, ..., rp = rp)
  return(x)
}

f_expect.arbitrary <- function(x, i, j, l) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  tf <- mean
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  rp <- x[i, j, , l] * -1
  x[i, j, , l] <- rp
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# test arbitrary dimensions with NA ====

subset_arr <- function(x, i, j, l, rp) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- rp
  return(x)
}

make_rp <- function(len) {
  return(sample(as.integer(c(seq_len(len)*-1, NA)), size = len))
}
x <- mutable_atomic(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

s <- list(c("b", "a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
len <- length(sb_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)


s <- list(c("b", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
len <- length(sb_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)

s <- list(c("b", "a"), c(1:4), rep(FALSE, 10))
d <- c(1,2,4)
len <- length(sb_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
enumerate <- enumerate + 6


# test errors ====

sb_test <- function(x, ...) {
  x <- as.mutable_atomic(x)
  sb_set(x, ..., rp = 1)
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())



sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb_set(x, ...)
  expect_equal(x, x2) |> errorfun()
  return(x)
}

x <- as.mutable_atomic(1:10)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed",
  fixed = TRUE
)
enumerate <- enumerate + 2


x <- as.mutable_atomic(matrix(1:10, nrow = 2))
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "replacement must be atomic"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, n(1:2), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 4


x <- as.mutable_atomic(array(1:27, dim = c(3,3,3)))
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "replacement must be atomic"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, n(1:2, 1:2), c(1,3), rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, n(1:2, 1:2), c(2,3), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 7



# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously

print(enumerate)

