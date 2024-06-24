
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE

sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb_set(x, ..., inv = TRUE)
  return(x)
}
sb_set2.array <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb_set.array(x, ..., inv = TRUE)
  return(x)
}

# test elements ====

test_sb <- function(x, i, rp) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_rm(i, x, names(x), length(x))
  if(length(i) == 0) return(x)
  x[i] <- rp
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1L, length(indx_rm(elements[[i]], x, names(x), length(x))))
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
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  return(x[row, col])
}

subset_mat <- function(x, row = NULL, col = NULL, rp) {
  
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col] <- rp
  
  return(x)
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
      rp <- sample(c(seq_len(len), NA), size = len)
      expect_equal(
        sb_set2(x, row = row[[i]], col = col[[j]], rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb_set2(x, row = row[[i]], col = col[[j]], rp = rp) |>
                    is.matrix()) |> errorfun()
      
      rp <- NA
      expect_equal(
        sb_set2(x, row = row[[i]], col = col[[j]], rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb_set2(x, row = row[[i]], col = col[[j]], rp = rp) |>
                    is.matrix()) |> errorfun()
      
      assign("enumerate", enumerate + 4, envir = parent.frame(n = 1))
    }
  }
}


subset_1d <- function(x, i, rp) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  i <- indx_rm(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  x[i] <- rp
  return(x)
}

temp.fun.1d <- function(x, row) {
  for(i in 1:length(row)) {
    rp <- seq_along(indx_rm(row[[i]], x, dimnames(x)[[1]], length(x)))
    
    expect_equal(
      sb_set2(x, row[[i]], 1, rp = rp),
      subset_1d(x, row[[i]], rp = rp)
    ) |> errorfun()
    expect_true(sb_set2(x, row[[i]], 1, rp = rp) |>
                  is.array()) |> errorfun()
    
    rp <- NA
    expect_equal(
      sb_set2(x, row[[i]], 1, rp = rp),
      subset_1d(x, row[[i]], rp = rp)
    ) |> errorfun()
    expect_true(sb_set2(x, row[[i]], 1, rp = rp) |>
                  is.array()) |> errorfun()
    
    assign("enumerate", enumerate + 4, envir = parent.frame(n = 1))
  }
}

temp.fun.2d <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
      rp <- sample(c(seq_len(len), NA), size = len)
      
      sub <- n(row[[i]], col[[j]])
      dims <- 1:2
      rem <- which(vapply(sub, is.null, logical(1L)))
      if(length(rem) > 0L) {
        sub <- sub[-rem]
        dims <- dims[-rem]
      }
      
      expect_equal(
        sb_set2.array(x, sub, dims, rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb_set2.array(x, sub, dims, rp = rp) |>
                    is.array()) |> errorfun()
      
      rp <- NA
      expect_equal(
        sb_set2.array(x, sub, dims, rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb_set2.array(x, sub, dims, rp = rp) |>
                    is.array()) |> errorfun()
      
      assign("enumerate", enumerate + 4, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- function(x, ...) {
  x <- as.mutable_atomic(x)
  rp <- sb_rm.array(x, sub, dims) * -1
  sb_set.array(x, ..., inv = TRUE, rp = rp)
  return(x)
}

temp.fun.arbitrary <- function(x, i, j, l) {
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  tf <- mean
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  rp <- x[i, j, , l] * -1
  x[i, j, , l] <- rp
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# test arbitrary dimensions with NA ====

subset_arr <- function(x, i, j, l, rp) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- rp
  return(x)
}

make_rp <- function(len) {
  return(sample(as.integer(c(seq_len(len)*-1, NA)), size = len))
}
x <- mutable_atomic(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
len <- length(sb_rm(x, sub, dims))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)


sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
len <- length(sb_rm(x, sub, dims))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)

sub <- list(c("a"), c(1:4), rep(FALSE, 10))
dims <- c(1,2,4)
len <- length(sb_rm(x, sub, dims))
rp <- make_rp(len)
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)
rp <- NA
expect_equal(
  sb_set2(x, sub, dims, rp = rp),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]], rp)
)
enumerate <- enumerate + 6


# test errors ====

sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb_set(x, ...)
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
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, row = 1:2, col = 1:2, rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 4


x <- as.mutable_atomic(array(1:27, dim = c(3,3,3)))
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, list(1:2, 1:2), c(1,3), rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, list(1:2, 1:2), c(2,3), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 5


# report number of tests

print(enumerate)

