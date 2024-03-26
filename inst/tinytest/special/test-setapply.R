
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


# error checks ====

margin <- 1

x <- matrix(1:20, ncol = 4)
expect_error(
  setapply(x, margin, rev),
  pattern = '`x` must be a mutable_atomic matrix'
)
x <- mutable_atomic(1:27, dim = c(3,3,3))
expect_error(
  setapply(x, margin, rev),
  pattern = '`x` must be a mutable_atomic matrix'
)

x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  setapply(x, margin, ~ 1),
  pattern = "`FUN` must be a function"
)

x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  setapply(x, margin, \(x)x[1]),
  pattern = "improper function given"
)
expect_error(
  setapply(x, margin, \(x)as.character(x)),
  pattern = "improper function given"
)
enumerate <- enumerate + 5

margin <- 2

x <- matrix(1:20, ncol = 4)
expect_error(
  setapply(x, margin, rev),
  pattern = '`x` must be a mutable_atomic matrix'
)
x <- mutable_atomic(1:27, dim = c(3,3,3))
expect_error(
  setapply(x, margin, rev),
  pattern = '`x` must be a mutable_atomic matrix'
)

x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  setapply(x, margin, ~ 1),
  pattern = "`FUN` must be a function"
)

x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  setapply(x, margin, \(x)x[1]),
  pattern = "improper function given"
)
expect_error(
  setapply(x, margin, \(x)as.character(x)),
  pattern = "improper function given"
)
enumerate <- enumerate + 5

x <- mutable_atomic(1:20, dim = c(5,4))
expect_error(
  setapply(x, 3, rev),
  pattern = "`MARGIN` must be 1 or 2"
)
enumerate <- enumerate + 1



# main checks ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 90, TRUE),
  sample(c(1:88, NA, NA)),
  rnorm(90),
  sample(c(NA, NaN, Inf, -Inf, 0), 90, TRUE),
  sample(c(letters, LETTERS, NA, NA), 90, TRUE),
  sample(c(as.complex(1:89), NA)),
  as.raw(0:89),
  rep(NA, 90)
)

# main functionality check

for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:nrow(x)) x2[i,] <- rev(x2[i,]) 
  setapply(x, 1, rev)
  invisible(x) # waking up R
  expect_equal(
    x, as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}

for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:ncol(x)) x2[,i] <- rev(x2[,i]) 
  setapply(x, 2, rev)
  expect_equal(
    x, as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# modify by reference check


for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  setapply(x, 1, rev)
  invisible(x) # waking up R
  expect_equal(
    x, x2
  ) |> errorfun()
  enumerate <- enumerate + 1
}

for(iD in 1:length(x.data)) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  setapply(x, 2, rev)
  expect_equal(
    x, x2
  ) |> errorfun()
  enumerate <- enumerate + 1
}
