
# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# test if the most simplest case works intuitively as expected

enumerate <- 0L

x <- mutable_atomic(1:1e7)
expect_equal(
  sum(x <= 10),
  countv(x, v = c(-Inf, 10))
)
expect_equal(
  x[x<=10],
  slicev_x(x, v = c(-Inf, 10))
)

x2 <- x
x2[x2 <= 10] <- -1L
slicev_set(x, v = c(-Inf, 10), rp = -1L)
expect_equal(
  x, x2
)

enumerate <- 3L

