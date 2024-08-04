
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
    sb_mod(x, rp = x[1], inv = TRUE),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




# test elements ====

test_sb <- function(x, i, rp) {
  if(!is.list(x)) {
    i <- indx_rm(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- rp
    return(x)
  }
  if(is.list(x)) {
    i <- indx_rm(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- as.list(rp)
    if(length(i) == 1) x[[i]] <- rp
    return(x)
  }
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1, length(indx_rm(elements[[i]], x, names(x), length(x))))
    if(is.list(x)) rp1 <- as.list(rp1)
    if(is.list(x) && length(rep) != 1) rp2 <- as.list(rp)
    expect_equal(
      sb_mod(x, i = elements[[i]], rp = rp1, inv = TRUE),
      test_sb(x, i = elements[[i]], rp = rp2)
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test factor ====
x <- factor(rep(c("Male", "Female", "Other", "Unknown"), 4))
names(x) <- c(letters[1:13], "a", "a", NA)
x2 <- x
levels(x2) <- c("F", "M", "Other", "Unknown")
expect_equal(sb_mod(x, lvl = c("Female", "Male"), rp = c("F", "M"), inv = TRUE), x2)

x <- factor(rep(c("Male", "Female", "Other", "Unknown"), 4))
names(x) <- c(letters[1:13], "a", "a", NA)
x2 <- x
x2[-1:-2] <-  c("Other")
expect_equal(sb_mod(x, i = 1:2, rp = "Other", inv = TRUE), x2)
expect_equal(sb_mod(x, i = c(TRUE, TRUE, rep(FALSE, 14)), rp = "Other", inv = TRUE), x2)

x <- factor(rep(c("Male", "Female", "Other", "Unknown"), 4))
names(x) <- c(letters[1:13], "a", "a", NA)
x2 <- x
x2[indx_rm("a",x, names(x), length(x))] <- "Male"
expect_equal(sb_mod(x, i = "a", rp = "Male", inv = TRUE), x2)


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
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col])
}


subset_mat <- function(x, row = NULL, col = NULL, rp) {
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col] <- rp
  
  return(x)
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
        len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
        rp <- seq_len(len)
        expect_equal(
          sb_mod(x, row = row[[i]], col = col[[j]], rp = rp, inv = TRUE),
          subset_mat(x, row[[i]], col[[j]], rp = rp)
        ) |> errorfun()
        expect_true(sb_mod(x, row = row[[i]], col = col[[j]], rp = rp, inv = TRUE) |>
                      is.matrix()) |> errorfun()
        assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


subset_1d <- function(x, i, rp) {
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
      sb_mod(x, row[[i]], 1, inv = TRUE, rp = rp),
      subset_1d(x, row[[i]], rp = rp)
    ) |> errorfun()
    expect_true(sb_mod(x, row[[i]], 1, inv = TRUE, rp = rp) |>
                  is.array()) |> errorfun()
    assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
  }
}

temp.fun.2d <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
      rp <- seq_len(len)
      
      sub <- n(row[[i]], col[[j]])
      dims <- 1:2
      rem <- which(vapply(sub, is.null, logical(1L)))
      if(length(rem) > 0L) {
        sub <- sub[-rem]
        dims <- dims[-rem]
      }
      
      expect_equal(
        sb_mod.array(x, sub, dims, inv = TRUE, rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb_mod.array(x, sub, dims, inv = TRUE, rp = rp) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- function(...) {
  rp <- sb_rm.array(...) * -1
  sb_mod.array(..., inv = TRUE, rp = rp)
}

temp.fun.arbitrary <- function(x, i, j, l) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  if(any_empty_indices(i, j, l)) {
    return(x)
  }
  rp <- x[i, j, , l] * -1
  x[i, j, , l] <- rp
  return(x)
}


sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test arbitrary dimensions ====
subset_arr <- function(x, i, j, l) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  rp <- seq_len(length(x[i, j, , l])) * -1
  x[i, j, , l] <- rp
  return(x)
}

x <- array(seq_len(5^4), dim = c(5, 5, 5, 5))
rownames(x) <- c(letters[1:3], "a", NA)

sub <- list(c("a"), c(1:3), c(rep(TRUE, 2), rep(FALSE, 3)))
dims <- c(1,2,4)
rp <- seq_len(length(sb_rm(x, sub, dims)))* -1
expect_equal(
  sb_mod(x, sub, dims, rp = rp, inv = TRUE),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]])
)

sub <- list(c("a"), logical(0), c(rep(TRUE, 2), rep(FALSE, 3)))
dims <- c(1,2,4)
rp <- seq_len(length(sb_rm(x, sub, dims)))* -1
expect_equal(
  sb_mod(x, sub, dims, rp = rp, inv = TRUE),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]])
)

sub <- list(c("a"), c(1:4), rep(FALSE, 5))
dims <- c(1,2,4)
rp <- seq_len(length(sb_rm(x, sub, dims)))* -1
expect_equal(
  sb_mod(x, sub, dims, rp = rp, inv = TRUE),
  subset_arr(x, sub[[1]], sub[[2]], sub[[3]])
)

enumerate <- enumerate + 3



# test errors ====

sb_test <- function(x, ...)sb_mod(x, ..., inv = TRUE, rp = x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())


x <- 1:10
expect_error(
  sb_mod(x, i = 1:5, rp = 1:6, inv = TRUE),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 2


x <- matrix(1:20, nrow = 4)
expect_error(
  sb_mod(x, i = 1:5, rp = as.list(1:5), inv = TRUE),
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_mod(x, i = 1:5, rp = 1:6, inv = TRUE),
  pattern = "recycling not allowed"
)
expect_error(
  sb_mod(x, row = 1:2, col = 1:2, rp = 1:7, inv = TRUE),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 4


x <- array(1:27, dim = c(3,3,3))
expect_error(
  sb_mod(x, i = 1:5, rp = as.list(1:5), inv = TRUE),
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_mod(x, i = 1:5, rp = 1:6, inv = TRUE),
  pattern = "recycling not allowed"
)
expect_error(
  sb_mod(x, list(1:2, 1:2), c(1,3), rp = 1:6, inv = TRUE),
  pattern = "recycling not allowed"
)
expect_error(
  sb_mod(x, list(1:2, 1:2), c(2,3), rp = 1:6, inv = TRUE),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 5


# report number of tests

print(enumerate)

