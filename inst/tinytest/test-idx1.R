
source(file.path(getwd(), "source", "functions4testing.R"))

enumerate <- 0

# NOTE:
# idx1.default() is just a copy of the element selection internal function
# used in all other methods,
# and so does not need to be tested separately here.
# Moreover,
# dx1.array() is called internally by sb_set.array(),
# and so does not need to be tested separately here.
# Thus, only idx1.matrix() is tested here.


# inv = FALSE ====

test_allow_duplicates <- FALSE
test_use_factors <- TRUE
test_PassByReference <- FALSE

subset_mat <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(as.vector(x[row, col]))
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      expect_equal(
        idx1(x, row = row[[i]], col = col[[j]]),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
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
  c(indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a")))
}



# uniquely named matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)


# unnamed matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.matrix(x, row, col)


# non-uniquely named matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "c", NA)
colnames(x) <- c("a", "a", "b", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)



# inv = TRUE ====


test_allow_duplicates <- FALSE
test_use_factors <- TRUE
test_PassByReference <- FALSE

subset_mat <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(as.vector(x[row, col]))
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      expect_equal(
        idx1(x, row = row[[i]], col = col[[j]], inv = TRUE),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
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



# uniquely named matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)


# unnamed matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.matrix(x, row, col)


# non-uniquely named matrix ====
x <- matrix(1:20, nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "c", NA)
colnames(x) <- c("a", "a", "b", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)


