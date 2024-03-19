
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


# error checks ====

x <- mutable_atomic(1:10)

expect_error(
  ma_setv(x, v = ~1, rp = 1),
  pattern = "`v` and  `rp` must be atomic scalars"
)
expect_error(
  ma_setv(x, v = 1, rp = ~1),
  pattern = "`v` and  `rp` must be atomic scalars"
)
expect_error(
  ma_setv(x, v = 1:10, rp = 1),
  pattern = "`v` and  `rp` must be atomic scalars"
)
expect_error(
  ma_setv(x, v = 1, rp = 1:10),
  pattern = "`v` and  `rp` must be atomic scalars"
)
expect_error(
  ma_setv(x, v = NA, rp = 1),
  pattern = "`NA`/ `NaN` not allowed for `v`"
)
expect_error(
  ma_setv(x, v = 1, rp = 1, NA.safety = NA),
  pattern = "`NA.safety` must be `TRUE` or `FALSE`"
)
expect_error(
  ma_setv(x, v = 1, rp = 1, invert = c(TRUE, FALSE, NA))
)
expect_error(
  ma_setv(x, v = 1, rp = 1, invert = NA)
)

enumerate <- enumerate + 8



# main checks ====

x.data <- list(
  sample(c(TRUE, FALSE, NA), 125, TRUE),
  sample(c(1:123, NA, NA)),
  c(2.5, rnorm(123), 2.5),
  sample(c(NA, NaN, -Inf, Inf, 0), 125, TRUE),
  sample(c(letters, LETTERS, NA, NA), 125, TRUE),
  as.complex(c(1:124, NA)),
  as.raw(0:124)
)

values <- list(
  TRUE, 2L, 2.5, Inf, "A", as.complex(3), as.raw(0)
)
rps <- list(
  FALSE, -2L, -2.5, -Inf, "XXX", as.complex(1), as.raw(1)
)


for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]])
  x2 <- x
  ind <- which(x == values[[iD]])
  x2[ ind ] <- rps[[iD]]
  ma_setv(x, values[[iD]], rps[[iD]])
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  
  x <- mutable_atomic(x.data[[iD]], dim = c(5, 5, 5))
  x2 <- x
  ind <- which(x == values[[iD]])
  x2[ ind ] <- rps[[iD]]
  ma_setv(x, values[[iD]], rps[[iD]])
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  
  
  x <- mutable_atomic(x.data[[iD]])
  x2 <- x
  ind <- which(x != values[[iD]])
  x2[ ind ] <- rps[[iD]]
  ma_setv(x, values[[iD]], rps[[iD]], invert = TRUE)
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  
  x <- mutable_atomic(x.data[[iD]], dim = c(5, 5, 5))
  x2 <- x
  ind <- which(x != values[[iD]])
  x2[ ind ] <- rps[[iD]]
  ma_setv(x, values[[iD]], rps[[iD]], invert = TRUE)
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  
  enumerate <- enumerate + 4
  
}


for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]])
  x2 <- x
  ma_setv(x, values[[iD]], rps[[iD]])
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  
}

