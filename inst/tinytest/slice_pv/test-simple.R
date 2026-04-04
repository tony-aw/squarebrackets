
# set-up ====
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

# test if the most simplest case works intuitively as expected

enumerate <- 0L

x <- mutatomic(1:1e7)
expect_equal(
  sum(x <= 10),
  countv(x, v = c(-Inf, 10))
)
expect_equal(
  x[x<=10],
  long_x(x, stride_pv(x, v = c(-Inf, 10)))
)

x2 <- x
x2[x2 <= 10] <- -1L
long_set(x, stride_pv(x, v = c(-Inf, 10)), rp = -1L)
expect_equal(
  x, x2
)

enumerate <- 3L

