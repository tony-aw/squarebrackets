
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

test_allow_duplicates <- TRUE
test_use_factors <- TRUE
test_PassByReference <- FALSE


# test missing arguments (NULL) ====

temp.fun <- function(x) {
  expect_equal(
    sb_x(x),
    x
  ) |> errorfun()
}

sys.source(file.path(getwd(), "source", "sourcetest-missingargs.R"), envir = environment())



# test elements ====

test_sb <- function(x, i) {
  i <- indx_x(i, x, names(x), length(x))
  return(x[i])
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb_x(x, i = elements[[i]]),
      test_sb(x, i = elements[[i]])
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test factors ====

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    expect_equal(
      sb_x(x, i = elements[[i]]),
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


# test matrix & arrays ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
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
          sb_x(x, row = row[[i]], col = col[[j]]),
          subset_mat(x, row[[i]], col[[j]])
        ) |> errorfun()
        expect_true(sb_x(x, row = row[[i]], col = col[[j]]) |>
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
      sb_x(x, row[[i]], 1),
      subset_1d(x, row[[i]])
    ) |> errorfun()
    expect_true(sb_x(x, row[[i]], 1) |>
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
        sb_x.array(x, sub, dims),
        subset_mat(x, row[[i]], col[[j]])
      ) |> errorfun()
      expect_true(sb_x.array(x, sub, dims) |>
                    is.array()) |> errorfun()
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
    }
  }
}


sb_test <- sb_x.array

temp.fun.arbitrary <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  return(x[i, j, , l, drop = FALSE])
}

sys.source(file.path(getwd(), "source", "sourcetest-dims.R"), envir = environment())



# test errors ====
sb_test <- sb_x
sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())



# report number of tests

print(enumerate)

