
# set-up ====
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}
enumerate <- 0L

# no match tests ====
x <- sample(1:10)
y <- sample(1:10)

expect_equal(
  long_x(x, stride_pv(y, v = 1000)),
  x[numeric(0)]
)
expect_equal(
  long_x(x, stride_pv(y, v = 1000), -1),
  x
)

out <- data.table::copy(x) |> as.mutatomic()
expected <- data.table::copy(out)
long_set(out, stride_pv(y, v = 1000), rp = -1)
expect_equal(
  out,
  expected
)

out <- data.table::copy(x) |> as.mutatomic()
expected <- data.table::copy(out)
long_set(out, stride_pv(y, v = 1000), -1, rp = -1)
expected[1:length(expected)] <- -1L
expect_equal(
  out,
  expected
)

enumerate <- enumerate + 5L


