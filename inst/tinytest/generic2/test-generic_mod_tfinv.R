
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- FALSE


# test missing arguments (NULL) ====

temp.fun <- function(x) {
  tempfun <- function(x) {
    x[] <- -1
    return(x)
  }
  expect_equal(
    sb2_mod(x, inv = TRUE, tf = \(x) return(-1)),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




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
      sb2_mod(x, i = elements[[i]], tf = min, inv = TRUE),
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



# test arbitrary dimensions ====


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
  x[row, col] <- lapply(x[row, col], tf)
  
  return(x)
}


subset_1d <- function(x, i) {
  tf <- mean
  i <- indx_rm(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  x[i] <- lapply(x[i], tf)
  return(x)
}

temp.fun.1d <- function(x, row) {
  for(i in 1:length(row)) {
    expect_equal(
      sb2_mod(x, row[[i]], 1, tf = mean),
      subset_1d(x, row[[i]])
    ) |> errorfun()
    expect_true(sb2_mod(x, row[[i]], 1, tf = mean) |>
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
        sb2_mod.array(x, sub, dims, tf = mean),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb2_mod.array(x, sub, dims, tf = mean) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- function(...) sb2_mod.array(..., inv = TRUE, tf = mean)

temp.fun.arbitrary <- function(x, i, j, l) {
  tf <- mean
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- lapply(x[i, j, , l], tf)
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test datasets ====

subset_df <- function(x, row, col, filter, get_vars) {
  
  tf <- function(x)x[1]
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- which(!model.frame(as.formula(filter), data = x)[, 1] |> as.logical())
  }
  if(!is.null(get_vars)) {
    col <- which(!sapply(x, get_vars))
  }
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  x[row, col] <- lapply(x[row, col, drop = FALSE], tf)
  
  return(x)
}

subset_dt <- function(x, row, col, filter, get_vars) {
  
  tf <- function(x)x[1]
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- which(!(model.frame(as.formula(filter), data = x)[, 1] |> as.logical()))
  }
  if(!is.null(get_vars)) {
    col <- which(!sapply(x, get_vars))
  }
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  row <- as.integer(row)
  col <- as.integer(col)
  
  value <- collapse::ss(x, row, col, check = FALSE)
  value <- lapply(value, tf)
  x <- data.table::copy(x)
  data.table::set(x, row, col, value)
  
  return(x)
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
              sb2_mod(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], tf = \(x)x[1], inv = TRUE),
              mysubset(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]])
            ) |> errorfun()
            expect_true(
              sb2_mod(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], tf = \(x)x[1], inv = TRUE) |> is.data.frame()
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
    1, 1:2, 2:1
  )
  return(out)
}

indx_named <- function(x, dim.i) {
  if(dim.i==1) return(c(indx_general(x, dim.i), list("1", c("1", "2"))))
  if(dim.i==2) return(c(indx_general(x, dim.i), list("a", c("a", "b"))))
}

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())



# test errors ====
sb_test <- function(...)sb2_mod(..., inv = TRUE, tf = \(x)x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())

expect_error(
  sb2_mod(as.list(1:10), i = 1, tf = "foo", inv = TRUE),
  pattern = "`tf` must be a function"
)

# report number of tests

print(enumerate)

