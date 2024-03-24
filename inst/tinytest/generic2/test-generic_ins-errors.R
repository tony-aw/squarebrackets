
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# general errors ====


x <- as.list(1:10)
new <- as.list(1)
expect_error(
  sb2_after(x, new, -1),
  pattern = "`pos` must be a strictly positive integer scalar",
  fixed = TRUE
) |> errorfun()
expect_error(
  sb2_after(x, new, 1000),
  pattern = "subscript out of bounds",
  fixed = TRUE
) |> errorfun()
enumerate <- enumerate + 3


x <- list(a = 1, b = 2)
expect_error(
  sb2_after(x, "c"),
  pattern = "`new` must be a (possibly named) list",
  fixed = TRUE
)
expect_error(
  sb2_before(x, "c"),
  pattern = "`new` must be a (possibly named) list",
  fixed = TRUE
)
x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  sb2_before(x, 1:10, 1),
  pattern = "`new` must be a data.frame-like object",
  fixed = TRUE
)
expect_error(
  sb2_before(x, 1:10, 2),
  pattern = "`new` must be a data.frame-like object",
  fixed = TRUE
)
expect_error(
  sb2_after(x, 1:10, 1),
  pattern = "`new` must be a data.frame-like object",
  fixed = TRUE
)
expect_error(
  sb2_after(x, 1:10, 2),
  pattern = "`new` must be a data.frame-like object",
  fixed = TRUE
)
enumerate <- enumerate + 8

