
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

# no match tests ====
x <- sample(1:10)
y <- sample(1:10)

expect_equal(
  countv(y, v = 1000),
  0
)
expect_equal(
  slicev_x(x, y = y, v = 1000),
  x[numeric(0)]
)
expect_equal(
  slicev_x(x, y = y, v = 1000, r = FALSE),
  x
)

out <- data.table::copy(x) |> mutatomic::as.mutatomic()
expected <- data.table::copy(out)
slicev_set(out, y = y, v = 1000, rp = -1)
expect_equal(
  out,
  expected
)

out <- data.table::copy(x) |> mutatomic::as.mutatomic()
expected <- data.table::copy(out)
slicev_set(out, y = y, v = 1000, r = FALSE, rp = -1)
expected[1:length(expected)] <- -1L
expect_equal(
  out,
  expected
)

enumerate <- enumerate + 5L


