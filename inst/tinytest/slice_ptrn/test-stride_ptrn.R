
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# errors - start,end ====

badparams <- list(NA, -1, 0, 1:10)
for(i in seq_along(badparams)) {
  
  y <- badparams[[i]]
  
  expect_error(
    stride_ptrn(y, 10, c(TRUE, FALSE, FALSE, TRUE)),
    pattern = "`start` and `end` must be natural scalars",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    stride_ptrn(1, y, c(TRUE, FALSE, FALSE, TRUE)),
    pattern = "`start` and `end` must be natural scalars",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
}

# errors - ptrn ====

expect_error(
  stride_ptrn(1, 10, c(TRUE, FALSE, NA)),
  pattern = "`ptrn` cannot have NAs"
)
expect_error(
  stride_ptrn(1, 10, TRUE),
  pattern = "`ptrn` must have at least 2 elements"
)
expect_error(
  stride_ptrn(1, 10, FALSE),
  pattern = "`ptrn` must have at least 2 elements"
)
expect_error(
  stride_ptrn(1, 10, rep(c(TRUE, FALSE), 10)),
  pattern = "`ptrn` may not be longer than the range start:end"
)
expect_error(
  stride_ptrn(1, 10, c(TRUE, TRUE)),
  pattern = "`ptrn` must contain at least one `TRUE` and one `FALSE` element"
)
expect_error(
  stride_ptrn(1, 10, c(FALSE, FALSE)),
  pattern = "`ptrn` must contain at least one `TRUE` and one `FALSE` element"
)
enumerate <- enumerate + 6L

