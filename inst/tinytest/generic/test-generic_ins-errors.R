
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# general errors ====
as_funs_lst <- list(
  as.vector,
  as.factor
)
x. <- 1:10
for(i in 1:length(as_funs_lst)) {
  x <- as_funs_lst[[i]](x.)
  new <- as_funs_lst[[i]](1)
  expect_error(
    sb_after(x, new, -1),
    pattern = "`pos` must be a strictly positive integer scalar",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    sb_after(x, new, 1000),
    pattern = "subscript out of bounds",
    fixed = TRUE
  ) |> errorfun()
  enumerate <- enumerate + 3
}


# class-specific errors ====
x <- factor(letters)
expect_error(
  sb_after(x, "a"),
  pattern = "`new` must be a (possibly named) factor",
  fixed = TRUE
)
expect_error(
  sb_before(x, "a"),
  pattern = "`new` must be a (possibly named) factor",
  fixed = TRUE
)

enumerate <- enumerate + 2

