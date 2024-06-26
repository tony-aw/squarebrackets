
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

test_allow_duplicates <- TRUE
test_use_factors <- TRUE
test_PassByReference <- FALSE

# test elements ====

test_sb <- function(x, i) {
  i <- indx_x(i, x, names(x), length(x))
  return(x[i])
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb2_x(x, i = elements[[i]], .attr = NULL),
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
  sb2_x(x, 1, drop = TRUE),
  x[[1]]
)
expect_equal(
  sb2_x(x, c(TRUE, rep(FALSE, 9)), drop = TRUE),
  x[[1]]
)
enumerate <- enumerate + 2



# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(bind2_array(n(x, x, x), along = dim))
}

subset_mat <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col, drop = FALSE])
}

temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      expect_equal(
        sb2_x(x, row = row[[i]], col = col[[j]]),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb2_x(x, row = row[[i]], col = col[[j]]) |>
                    is.matrix()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


subset_1d <- function(x, i) {
  i <- indx_x(i, x, dimnames(x)[[1]], length(x))
  return(x[i, drop = FALSE])
}

temp.fun.1d <- function(x, row) {
  for(i in 1:length(row)) {
    expect_equal(
      sb2_x(x, row[[i]], 1),
      subset_1d(x, row[[i]])
    ) |> errorfun()
    expect_true(sb2_x(x, row[[i]], 1) |>
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
        sb2_x.array(x, sub, dims),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb2_x.array(x, sub, dims) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- sb2_x.array

temp.fun.arbitrary <- function(x, i, j, l) {
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

sub <- list("b", 1, 1, 2)
dims <- 1:4
expect_equal(
  sb2_x(x, sub, dims, drop = TRUE),
  x[["b", 1, 1, 2]]
)

enumerate <- enumerate + 1


# test datasets ====

subset_df <- function(x, row, col, filter, get_vars) {
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- model.frame(as.formula(filter), data = x)[, 1] |> as.logical()
  }
  if(!is.null(get_vars)) {
    col <- which(sapply(x, get_vars))
  }
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col, drop = FALSE]
}

subset_dt <- function(x, row, col, filter, get_vars) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- model.frame(as.formula(filter), data = x)[, 1] |> as.logical()
  }
  if(!is.null(get_vars)) {
    col <- which(sapply(x, get_vars))
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
              sb2_x(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]),
              mysubset(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]])
            ) |> errorfun()
            expect_true(
              sb2_x(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]) |> is.data.frame()
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
    1, 1:2, 2:1, c(1, 1, 1)
  )
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i==1) return(c(indx_general(x, dim.i), list("1", c("1", "2"), c("1", "1", "1"))))
  if(dim.i==2) return(c(indx_general(x, dim.i), list("a", c("a", "b"), c("a", "a", "a"))))
}

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====
sb_test <- sb2_x
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())



# report number of tests

print(enumerate)

