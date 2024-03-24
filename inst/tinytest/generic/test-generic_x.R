
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
      sb_x(x, i = elements[[i]], .attr = NULL),
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


# test factors ====

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb_x(x, i = elements[[i]], .attr = NULL),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    expect_equal(
      sb_x(x, i = elements[[i]], drop = TRUE),
      test_sb(x, i = elements[[i]], drop = TRUE)
    ) |> errorfun()
    assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
  }
}


test_sb <- function(x, i = NULL, lvl = NULL, drop = FALSE) {
  if(!is.null(i)) {
    i <- indx_x(i, x, names(x), length(x))
  }
  if(!is.null(lvl)) {
    i <- lapply(
      lvl, \(i) which(x == i)
    ) |> unlist()
  }
  return(x[i, drop = drop])
}

indx_general <- list(
  logical(0),
  1, 1:2, 2:1, c(1, 1, 1), 
  function(x) x != "Jan"
)

indx_named <- c(indx_general, list("a", c("a", "b")))


sys.source(file.path(getwd(), "source", "sourcetest-factors.R"), envir = environment())

expect_equal(sb_x(x, lvl = "Jan"), x[x == "Jan"])
expect_equal(sb_x(x, lvl = "Jan", drop = TRUE), x[x == "Jan", drop = TRUE])
expect_equal(sb_x(x, lvl = "Jan"), x[x == "Jan"])
expect_equal(sb_x(x, lvl = "Jan", drop = TRUE), x[x == "Jan", drop = TRUE])
expect_equal(sb_x(x, lvl = "Jan"), x[x == "Jan"])
expect_equal(sb_x(x, lvl = "Jan", drop = TRUE), x[x == "Jan", drop = TRUE])
enumerate <- enumerate + 6


# test matrix & 3d array ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}

subset_mat <- function(x, row = NULL, col = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col, drop = FALSE]
}

subset_3darray <- function(x, row = NULL, col = NULL, lyr = NULL) {
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  if(!is.null(lyr)) lyr <- indx_x(lyr, x, dimnames(x)[[3]], dim(x)[3])
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  if(is.null(lyr)) lyr <- base::quote(expr = )
  x[row, col, lyr, drop = FALSE]
}

temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
        expect_equal(
          sb_x(x, row = row[[i]], col = col[[j]]),
          subset_mat(x, row[[i]], col[[j]])
        ) |> errorfun()
        expect_true(sb_x(x, row = row[[i]], col = col[[j]]) |>
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
          sb_x(x, rcl = list(row[[i]], col[[j]], lyr[[k]])),
          subset_3darray(x, row[[i]], col[[j]], lyr[[k]])
        ) |> errorfun()
        expect_true(sb_x(x, rcl = list(row[[i]], col[[j]], lyr[[k]])) |>
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
    1,1:2, 2:1, c(1, 1, 1)
  )
}

indx_named <- function(x, dim.i) {
  c(indx_general(x, dim.i), list("a", c("a", "b")))
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())


# test arbitrary dimensions ====

subset_arr <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l, drop = FALSE]
}

x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

idx <- list(c("a", "a"), c(1, 1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_x(x, idx, dims),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)

idx <- list(c("a", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_x(x, idx, dims),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)

idx <- list(c("a", "a"), c(1, 1:4), rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_x(x, idx, dims),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)
enumerate <- enumerate + 3


# test errors ====
sb_test <- sb_x
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())



# report number of tests

print(enumerate)

