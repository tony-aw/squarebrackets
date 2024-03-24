
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# lists ====
x <- list(a = seq(1.5, 10.5, by = 1), b = letters)
expect_equal(
  sb2_coe(x, 1, as.integer),
  {x[[1]] <- as.integer(x[[1]]); x}
)


# dimensional lists ====
x <- as.list(1:27) |> array(dim = c(3,3,3))
expect_equal(
  sb2_coe(x, idx = n(1:2), dims = 1, v = as.integer),
  {x[1:2, , ] <- as.list(x[1:2,,]); x}
)




# data.frame-like ====
obj <- data.table::data.table(
  a = seq(1.5, 10.5, by = 1),
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj1 <- data.table::data.table(
  a = as.integer(1:10),
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj2 <- sb2_coe(obj, vars = is.numeric, v = as.integer)
expect_equal(
  obj1,
  obj2
)


enumerate <- enumerate + 5

