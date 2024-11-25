
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
test_PassByReference <- TRUE


sb2_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb2_set(x, ..., inv = TRUE)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}


# test datasets ====


pre_subset_df <- sb2_rm.data.frame

f_expect.data.frame <- function(x, row = NULL, col = NULL, filter = NULL, get_vars = NULL) {
  
  rp <- parent.frame()$rp
  
  if(!is.null(row)) row <- indx_rm(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_rm(col, x, names(x), ncol(x))
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
  return(sb2_set2(x, row = row, col = col, filter = filter, vars = get_vars, rp = rp))
  
}


# rl. <- loadNamespace("rlang")
dt. <- loadNamespace("data.table")

sys.source(file.path(getwd(), "source", "sourcetest-datasets.R"), envir = environment())


# test errors ====

sb_test <- function(x, ...) {
  x <- data.table::copy(x)
  if(is.atomic(x)) x <- as.mutable_atomic(x)
  x2 <- x
  sb2_set(x, ..., inv = TRUE, rp = x[1])
  expect_equal(x, x2) |> errorfun()
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-errors.R"), envir = environment())



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



# report number of tests
enumerate <- enumerate * 2 # pass-by-reference mechanism was also tested simultaneously

print(enumerate)

