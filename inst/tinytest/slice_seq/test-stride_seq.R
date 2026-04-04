
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# vector checks ====
for(iFrom in 1:10) {
  for(iTo in 1:10) {
    for(iBy in 1:10) {
      
      from <- iFrom
      to <- iTo
      by <- ifelse(from > to, -iBy, iBy)
      x <- seq(from, to, by)
      stride <- stride_seq(from, to, by)
      
      expect_equal(
        c(x[1], x[length(x)]), as.numeric(stride[1:2])
      ) |> errorfun()
      expect_equal(
        length(x), stride$n_tiles
      ) |> errorfun()
      
      enumerate <- enumerate + 2L
    }
  }
}

# errors ====

expect_error(
  stride_seq(-1, 10, 1),
  pattern = "`from`, `to`, and `by` must be natural scalars",
  fixed = TRUE
)
expect_error(
  stride_seq(1, -1, 1),
  pattern = "`from`, `to`, and `by` must be natural scalars",
  fixed = TRUE
)

enumerate <- enumerate + 2L

badparams <- list(NA, 0, 1:10)
for(i in seq_along(badparams)) {
  
  y <- badparams[[i]]
  
  expect_error(
    stride_seq(y, 10, 1),
    pattern = "`from`, `to`, and `by` must be natural scalars",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    stride_seq(1, y, 1),
    pattern = "`from`, `to`, and `by` must be natural scalars",
    fixed = TRUE
  ) |> errorfun()
  expect_error(
    stride_seq(1, 10, y),
    pattern = "`from`, `to`, and `by` must be natural scalars",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}

expect_error(
  stride_seq(1, 10, 2^53),
  pattern = "`by` must not be larger than `2^31 - 1`",
  fixed = TRUE
)
enumerate <- enumerate + 4L

