
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
    sb2_mod(x, rp = x[1], inv = TRUE),
    tempfun(x)
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())




# test elements ====

test_sb <- function(x, i, rp) {
  if(!is.list(x)) {
    i <- indx_wo(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- rp
    return(x)
  }
  if(is.list(x)) {
    i <- indx_wo(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- as.list(rp)
    if(length(i) == 1) x[[i]] <- rp
    return(x)
  }
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1, length(indx_wo(elements[[i]], x, names(x), length(x))))
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

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}

pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  return(x[row, col])
}


f_expect.matrix <- f_expect.2d <- function(x, row = NULL, col = NULL) {
  
  rp <- parent.frame()$rp
  
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
  
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
  
  return(sb2_mod(x, row = row, col = col, inv = TRUE, rp = rp))
}

f_out.2d <- function(x, s, d) {
  
  rp <- parent.frame()$rp
  
  return(sb2_mod.array(x, s, d, inv = TRUE, rp = rp))
}


pre_subset_1d <- function(x, i) {
  return(indx_wo(i, x, names(x), length(x)))
}

f_expect.1d <- function(x, i) {
  
  rp <- parent.frame()$rp
  
  i <- indx_wo(i, x, dimnames(x)[[1]], length(x))
  
  if(any_empty_indices(i)) {
    return(x)
  }
  
  x[i] <- rp
  return(x)
}

f_out.1d <- function(x, s, d) {
  
  rp <- parent.frame()$rp
  
  return(sb2_mod(x, s, d, inv = TRUE, rp = rp))
}


sb_test <- function(x, ...) {
  rp <- lapply(sb2_wo.array(x, ...), \(x) x * -1)
  return(sb2_mod.array(x, ..., inv = TRUE, rp = rp))
}

f_expect.arbitrary <- function(x, i, j, l) {
  tf <- mean
  i <- indx_wo(i, x, rownames(x), nrow(x))
  j <- indx_wo(j, x, colnames(x), ncol(x))
  l <- indx_wo(l, x, dimnames(x)[4], dim(x)[4])
  rp <- lapply(x[i, j, , l], \(x) x * -1)
  x[i, j, , l] <- rp
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# 
# # test matrix & array ====
# 
# 
# pre_subset_mat <- function(x, row = NULL, col = NULL) {
#   
#   if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
#   if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
#   
#   if(any_empty_indices(row, col)) {
#     return(x)
#   }
#   
#   if(is.null(row)) row <- base::quote(expr = )
#   if(is.null(col)) col <- base::quote(expr = )
#   return(x[row, col])
# }
# 
# subset_mat <- function(x, row = NULL, col = NULL, rp) {
#   
#   if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
#   if(!is.null(col)) col <- indx_wo(col, x, colnames(x), ncol(x))
#   
#   if(any_empty_indices(row, col)) {
#     return(x)
#   }
#   
#   if(is.null(row)) row <- base::quote(expr = )
#   if(is.null(col)) col <- base::quote(expr = )
#   x[row, col] <- rp
#   
#   return(x)
# }
# 
# temp.fun.matrix <- function(x, row, col) {
#   for(i in 1:length(row)) {
#     for(j in 1:length(col)) {
#       len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
#       rp <- as.list(seq_len(len))
#       expect_equal(
#         sb2_mod(x, row = row[[i]], col = col[[j]], inv = TRUE, rp = rp),
#         subset_mat(x, row[[i]], col[[j]], rp = rp)
#       ) |> errorfun()
#       expect_true(sb2_mod(x, row = row[[i]], col = col[[j]], inv = TRUE, rp = rp) |>
#                     is.matrix()) |> errorfun()
#       assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
#     }
#   }
# }
# 
# 
# subset_1d <- function(x, i, rp) {
#   i <- indx_wo(i, x, dimnames(x)[[1]], length(x))
#   if(any_empty_indices(i)) {
#     return(x)
#   }
#   x[i] <- rp
#   return(x)
# }
# 
# temp.fun.1d <- function(x, row) {
#   for(i in 1:length(row)) {
#     rp <- as.list(seq_along(indx_wo(row[[i]], x, dimnames(x)[[1]], length(x))))
#     expect_equal(
#       sb2_mod(x, row[[i]], 1, inv = TRUE, rp = rp),
#       subset_1d(x, row[[i]], rp = rp)
#     ) |> errorfun()
#     expect_true(sb2_mod(x, row[[i]], 1, inv = TRUE, rp = rp) |>
#                   is.array()) |> errorfun()
#     assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
#   }
# }
# 
# temp.fun.2d <- function(x, row, col) {
#   for(i in 1:length(row)) {
#     for(j in 1:length(col)) {
#       
#       len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
#       rp <- as.list(seq_len(len))
#       
#       sub <- n(row[[i]], col[[j]])
#       dims <- 1:2
#       rem <- which(vapply(sub, is.null, logical(1L)))
#       if(length(rem) > 0L) {
#         sub <- sub[-rem]
#         dims <- dims[-rem]
#       }
#       
#       expect_equal(
#         sb2_mod.array(x, s, d, inv = TRUE, rp = rp),
#         subset_mat(x, row[[i]], col[[j]], rp = rp)
#       ) |> errorfun()
#       expect_true(sb2_mod.array(x, s, d, inv = TRUE, rp = rp) |>
#                     is.array()) |> errorfun()
#       assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
#     }
#   }
# }
# 
# 
# sb_test <- function(...) {
#   rp <- lapply(sb2_wo.array(...), \(x) x* - 1)
#   sb2_mod.array(..., inv = TRUE, rp = rp)
# }
# 
# temp.fun.arbitrary <- function(x, i, j, l) {
#   i <- indx_wo(i, x, rownames(x), nrow(x))
#   j <- indx_wo(j, x, colnames(x), ncol(x))
#   l <- indx_wo(l, x, dimnames(x)[4], dim(x)[4])
#   if(any_empty_indices(i, j, l)) {
#     return(x)
#   }
#   rp <- lapply(x[i, j, , l], \(x)x * -1)
#   x[i, j, , l] <- rp
#   return(x)
# }
# 
# sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())
# 

# test datasets ====

pre_subset_df <- sb2_wo.data.frame
f_expect.data.frame <- function(x, row = NULL, col = NULL, filter = NULL, get_vars = NULL) {
  
  rp <- parent.frame()$rp
  
  if(!is.null(row)) row <- indx_wo(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_wo(col, x, names(x), ncol(x))
  if(!is.null(filter)) {
    row <- model.frame(as.formula(filter), data = x)[, 1] |> as.logical()
    row <- which(!row)
  }
  if(!is.null(get_vars)) {
    col <- which(!vapply(x, get_vars, logical(1L)))
  }
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(nrow(x))
  if(is.null(col)) col <- seq_len(ncol(x))
  
  
  row <- as.integer(row)
  col <- as.integer(col)
  
  x <- data.table::copy(x)
  data.table::set(x, row, col, rp)
  
  return(x)
}

f_out.data.frame <- function(x, row = NULL, col = NULL, filter = NULL, get_vars = NULL) {
  
  rp <- parent.frame()$rp
  return(sb2_mod.data.frame(x, row, col, filter, get_vars, rp = rp, inv = TRUE))
  
}



# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====

sb_test <- function(x, ...)sb2_mod(x, ..., inv = TRUE, rp = x[1])
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())


x <- as.list(1:10)
expect_error(
  sb2_mod(x, i = 1:5, rp = 1:10, inv = TRUE),
  pattern = "replacement must be a list"
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


# report number of tests

print(enumerate)

