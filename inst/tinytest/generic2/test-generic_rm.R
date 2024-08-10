
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- TRUE
test_PassByReference <- FALSE



# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    sb2_rm(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())





# test elements ====


test_sb <- function(x, i) {
  i <- indx_rm(i, x, names(x), length(x))
  return(x[i])
}


temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb2_rm(x, i = elements[[i]]),
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
  sb2_rm(x, 1:9, drop = TRUE),
  x[[10]]
)
expect_equal(
  sb2_rm(x, c(FALSE, rep(TRUE, 9)), drop = TRUE),
  x[[1]]
)
enumerate <- enumerate + 2




# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(bind2_array(n(x, x, x), along = dim))
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

f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col, drop = FALSE])
}
f_out.matrix <- sb2_rm
f_out.2d <- sb2_rm.array
f_expect.1d <- function(x, i) {
  i <- indx_rm(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}
f_out.1d <- sb2_rm


pre_subset_1d <- function(x, i) {
  return(indx_rm(i, x, names(x), length(x)))
}


sb_test <- sb2_rm.array

f_expect.arbitrary <- function(x, i, j, l) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test arbitrary dimensions ====


x <- array(as.list(seq_len(10^4)), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

sub <- list(letters, 1:9, 1:9, 2:10)
dims <- 1:4
expect_equal(
  sb2_rm(x, sub, dims, drop = TRUE),
  x[[10, 10, 10, 1]]
)

enumerate <- enumerate + 1



# test datasets ====

subset_df <- function(x, row, col, filter, get_vars) {
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- which(!model.frame(as.formula(filter), data = x)[, 1] |> as.logical())
  }
  if(!is.null(get_vars)) {
    col <- which(!sapply(x, get_vars))
  }
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col, drop = FALSE]
}

subset_dt <- function(x, row, col, filter, get_vars) {
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- which(!(model.frame(as.formula(filter), data = x)[, 1] |> as.logical()))
  }
  if(!is.null(get_vars)) {
    col <- which(!sapply(x, get_vars))
  }
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  x[row, col, with = FALSE]
}

temp.fun.main <- function(x, row, col, filter, get_vars) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      for(k in 1:length(filter)) {
        for(l in 1:length(get_vars)) {
          wrong1 <- is.null(row[[i]]) && is.null(col[[j]]) && is.null(filter[[k]]) && is.null(get_vars[[l]])
          wrong2 <- !is.null(filter[[k]]) && !is.null(row[[i]])
          wrong3 <- !is.null(get_vars[[l]]) && !is.null(col[[j]])
          if(!wrong1 && !wrong2 && !wrong3) {
            cat(i, j, k, l)
            if(dt.$is.data.table(x)) {mysubset <- subset_dt} else{mysubset <- subset_df}
            expect_equivalent(
              sb2_rm(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]),
              mysubset(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]])
            ) |> errorfun()
            expect_true(
              sb2_rm(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]) |> is.data.frame()
            )
            assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
          }
        }
      }
    }
  }
}


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")

indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    NULL,
    logical(0),
    rep(TRUE, dim.n), rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1, seq(1, 2, by = 1), seq(2, 1, by = -1)
  )
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i==1) return(c(indx_general(x, dim.i), list("1", c("1", "2"))))
  if(dim.i==2) return(c(indx_general(x, dim.i), list("a", c("a", "b"))))
}


sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====
sb_test <- sb2_rm
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())


# report number of tests

print(enumerate)

