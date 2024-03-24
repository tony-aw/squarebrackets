
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE
any_empty_indices <- squarebrackets:::.any_empty_indices


sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb_set(x, ...)
  return(x)
}

# test elements ====

test_sb <- function(x, i) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_x(i, x, names(x), length(x))
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


# test matrix & 3d array ====

rep3.bind <- function(x, dim) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  return(abind::abind(x, x, x, along = dim))
}

subset_mat <- function(x, row = NULL, col = NULL) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  
  tf <- mean
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col] <- tf(x[row, col])
  
  return(x)
}


subset_3darray <- function(x, row = NULL, col = NULL, lyr = NULL) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  tf <- mean
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  if(!is.null(lyr)) lyr <- indx_x(lyr, x, dimnames(x)[[3]], dim(x)[3])
  
  if(any_empty_indices(row, col, lyr)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  if(is.null(lyr)) lyr <- seq_len(dim(x)[3])
  x[row, col, lyr] <- tf(x[row, col, lyr])
  
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


temp.fun.3darray <- function(x, row, col, lyr) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      for(k in 1:length(lyr)) {
        expect_equal(
          sb_set2(x, rcl = list(row[[i]], col[[j]], lyr[[k]]), tf = mean),
          subset_3darray(x, row[[i]], col[[j]], lyr[[k]])
        ) |> errorfun()
        expect_true(sb_set2(x, rcl = list(row[[i]], col[[j]], lyr[[k]]), tf = mean) |>
                      is.array()) |> errorfun()
        assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
      }
    }
  }
}

indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  list(
    NULL,
    logical(0),
    rep(TRUE, dim.n), rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1, seq(1, 2, by = 1), seq(2, 1, by = -1)
  )
}

indx_named <- function(x, dim.i) {
  c(indx_general(x, dim.i), list("a", c("a", "b")))
}


sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test arbitrary dimensions ====

subset_arr <- function(x, i, j, l, tf) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- tf(x[i, j, , l, drop = FALSE])
  return(x)
}


x <- as.mutable_atomic(array(seq_len(10^4), dim = c(10, 10, 10, 10)))
rownames(x) <- c(letters[1:8], "a", NA)
tf <- function(x) -x

idx <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, idx, dims, tf = tf),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
)

idx <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, idx, dims, tf = tf),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
)

idx <- list(c("a"), c(1:4), rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_set2(x, idx, dims, tf = tf),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]], tf)
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


# report number of tests

print(enumerate)

