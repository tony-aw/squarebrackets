
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


ii_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  ii_set(x, ...)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}
ss_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  ss_set(x, ...)
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
    ii_set2(x, rp = x[1]),
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
    ss_set2(x, rp = x[1]),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i, rp) {
  if(is.atomic(x)) x <- as.mutatomic(x)
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
      ii_set2(x, i = elements[[i]], rp = rp1),
      test_sb(x, i = elements[[i]], rp = rp2)
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())



test_sb <- function(x, i, rp) {
  if(is.atomic(x)) x <- as.mutatomic(x)
  i <- indx_wo(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- rp
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1L, length(indx_wo(elements[[i]], x, names(x), length(x))))
    expect_equal(
      ii_set2(x, elements[[i]], -1, rp = rp1),
      test_sb(x, elements[[i]], rp = rp2)
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
  
  rp <- parent.frame()$rp
  
  if(is.atomic(x)) x <- as.mutatomic(x)
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))

  if(any_empty_indices(i)) {
    return(x)
  }

  x[i] <- rp
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  rp <- parent.frame()$rp
  
  return(ss_set2(x, s, d, rp = rp))
}


sb_test <- function(x, ...) {
  x <- as.mutatomic(x)
  rp <- ss_x.default(x, ...) * -1
  ss_set(x, ..., rp = rp)
  return(x)
}

f_expect.arbitrary <- function(x, i, j, l) {
  if(is.atomic(x)) x <- as.mutatomic(x)
  tf <- mean
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  rp <- x[i, j, , l] * -1
  x[i, j, , l] <- rp
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# row,col ====


f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  
  x <- as.mutatomic(x)
  
  rp <- parent.frame()$rp
  
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

pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  sbt_x(x, row, col)
}

f_out.matrix <- f_out.2d <- function(x, row, col) {
  
  x <- as.mutatomic(x)
  rp <- parent.frame()$rp
  sbt_set(x, row, col, rp = rp)
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-rowcol.R"), envir = environment())



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
x <- mutatomic(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

s <- list(c("b", "a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
len <- length(ss_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)


s <- list(c("b", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
len <- length(ss_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)

s <- list(c("b", "a"), c(1:4), rep(FALSE, 10))
d <- c(1,2,4)
len <- length(ss_x(x, s, d))
rp <- make_rp(len)
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
rp <- NA
expect_equal(
  ss_set2(x, s, d, rp = rp),
  subset_arr(x, s[[1]], s[[2]], s[[3]], rp)
)
enumerate <- enumerate + 6


# test errors ====

sb_test <- function(x, ...) {
  x <- as.mutatomic(x)
  ii_set(x, ..., rp = 1)
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors-i.R"), envir = environment())


sb_test <- function(x, ...) {
  x <- as.mutatomic(x)
  ss_set(x, ..., rp = 1)
  return(x)
}
sys.source(file.path(getwd(), "source", "sourcetest-errors-ss.R"), envir = environment())



sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutatomic(x)
  x2 <- x
  sb_set(x, ...)
  expect_equal(x, x2) |> errorfun()
  return(x)
}

x <- as.mutatomic(1:10)
expect_error(
  ii_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed",
  fixed = TRUE
)
enumerate <- enumerate + 2


x <- as.mutatomic(matrix(1:10, nrow = 2))
expect_error(
  ii_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "replacement must match `is.atomic(x)` and `is.list(x)`",
  fixed = TRUE
)
expect_error(
  ii_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  ss_set2(x, n(1:2), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 4


x <- as.mutatomic(array(1:27, dim = c(3,3,3)))
expect_error(
  ii_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "replacement must match `is.atomic(x)` and `is.list(x)`",
  fixed = TRUE
)
expect_error(
  ii_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  ss_set2(x, n(1:2, 1:2), c(1,3), rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  ss_set2(x, n(1:2, 1:2), c(2,3), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 7



# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously

print(enumerate)

