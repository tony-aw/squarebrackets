
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


sb2_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb2_set(x, ...)
  return(x)
}


# test datasets ====


subset_dt <- function(x, row, col, filter, get_vars, rp) {
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  if(!is.null(filter)) {
    row <- which((model.frame(as.formula(filter), data = x)[, 1] |> as.logical()))
  }
  if(!is.null(get_vars)) {
    col <- which(sapply(x, get_vars))
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
            rp <- lapply(sb2_x(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]]), \(x)x[1])
            expect_equivalent(
              sb2_set2(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp),
              subset_dt(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp)
            ) |> errorfun()
            expect_true(
              sb2_set2(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp) |> is.data.frame()
            )
            
            rp <- list(NA)
            expect_equivalent(
              sb2_set2(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp),
              subset_dt(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp)
            ) |> errorfun()
            expect_true(
              sb2_set2(x, row[[i]], col[[j]], filter[[k]], get_vars[[l]], rp = rp) |> is.data.frame()
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
  if(dim.i==1) return(c(indx_general(x, dim.i), list("1", c("1", "2"), c("2", "1"))))
  if(dim.i==2) return(c(indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))))
}

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====

sb_test <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  sb2_set(x, ...)
  return(x)
}


if(requireNamespace("tidytable")) {
  xlist <- list(
    dt = data.table::data.table(a = 1:26, b = letters),
    tt = tidytable::tidytable(a = 1:26, b = letters)
  )
} else {
  xlist <- list(
    dt = data.table::data.table(a = 1:26, b = letters)
  )
}


for(i in 1:length(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, filter = "foo"),
    pattern = "`filter` must be a formula"
  ) |> errorfun()
  expect_error(
    sb_test(x, filter = ~ mean(a)),
    pattern = "invalid formula given"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = mean),
    pattern = "values must be type 'logical'"
  ) |> errorfun()
  enumerate <- enumerate + 5
}

for (i in 1:length(xlist)) {
  x <- xlist[[i]]
  colnames(x) <- c("a", "a")
  expect_error(
    sb_test(x, col=1),
    pattern = "`x` does not have unique variable names for all columns; \n fix this before subsetting"
  ) |> errorfun()
  enumerate <- enumerate + 1
}


x <- data.table::data.table(a = 1:10, b = letters[1:10])
expect_error(
  sb2_set2(x, row = 1:5, rp = 1:10),
  pattern = "`rp` must be a data.frame-like object or a list"
)
enumerate <- enumerate + 2



# report number of tests

print(enumerate)

