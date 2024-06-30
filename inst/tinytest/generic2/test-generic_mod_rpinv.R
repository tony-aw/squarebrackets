
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- FALSE


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
      sb2_mod(x, i = elements[[i]], rp = rp1, inv = TRUE),
      test_sb(x, i = elements[[i]], rp = rp2)
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}



indx_named <- c("ab")

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())




# test matrix & array ====


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
      rp <- as.list(seq_len(len))
      expect_equal(
        sb2_mod(x, row = row[[i]], col = col[[j]], inv = TRUE, rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb2_mod(x, row = row[[i]], col = col[[j]], inv = TRUE, rp = rp) |>
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
    rp <- as.list(seq_along(indx_rm(row[[i]], x, dimnames(x)[[1]], length(x))))
    expect_equal(
      sb2_mod(x, row[[i]], 1, inv = TRUE, rp = rp),
      subset_1d(x, row[[i]], rp = rp)
    ) |> errorfun()
    expect_true(sb2_mod(x, row[[i]], 1, inv = TRUE, rp = rp) |>
                  is.array()) |> errorfun()
    assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
  }
}

temp.fun.2d <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
      rp <- as.list(seq_len(len))
      
      sub <- n(row[[i]], col[[j]])
      dims <- 1:2
      rem <- which(vapply(sub, is.null, logical(1L)))
      if(length(rem) > 0L) {
        sub <- sub[-rem]
        dims <- dims[-rem]
      }
      
      expect_equal(
        sb2_mod.array(x, sub, dims, inv = TRUE, rp = rp),
        subset_mat(x, row[[i]], col[[j]], rp = rp)
      ) |> errorfun()
      expect_true(sb2_mod.array(x, sub, dims, inv = TRUE, rp = rp) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- function(...) {
  rp <- lapply(sb2_rm.array(...), \(x) x* - 1)
  sb2_mod.array(..., inv = TRUE, rp = rp)
}

temp.fun.arbitrary <- function(x, i, j, l) {
  i <- indx_rm(i, x, rownames(x), nrow(x))
  j <- indx_rm(j, x, colnames(x), ncol(x))
  l <- indx_rm(l, x, dimnames(x)[4], dim(x)[4])
  if(any_empty_indices(i, j, l)) {
    return(x)
  }
  rp <- lapply(x[i, j, , l], \(x)x * -1)
  x[i, j, , l] <- rp
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
            rp <- lapply(sb2_rm(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]), \(x)x[1])
            expect_equivalent(
              sb2_mod(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp, inv = TRUE),
              mysubset(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]])
            ) |> errorfun()
            expect_true(
              sb2_mod(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp, inv = TRUE) |> is.data.frame()
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

x <- as.list(1:10)
expect_error(
  sb2_mod(x, i = 1:5, rp = 1:10, inv = TRUE),
  pattern = "`rp` must be a list"
)
expect_error(
  sb2_mod(x, i = 1:5, rp = as.list(1:6), inv = TRUE),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 3


x <- array(as.list(1:27), dim = c(3,3,3))
expect_error(
  sb2_mod(x, i = 1:5, rp = as.list(1:6), inv = TRUE),
  pattern = "recycling not allowed"
)
expect_error(
  sb2_mod(x, list(1:2, 1:2), c(1,3), rp = as.list(1:6), inv = TRUE),
  pattern = "recycling not allowed"
)
expect_error(
  sb2_mod(x, list(1:2, 1:2), c(2,3), rp = as.list(1:6), inv = TRUE),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 5


x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  sb2_mod(x, row = 1:5, rp = 1:10, inv = TRUE),
  pattern = "`rp` must be a data.frame-like object or a list"
)
enumerate <- enumerate + 2


# report number of tests

print(enumerate)

