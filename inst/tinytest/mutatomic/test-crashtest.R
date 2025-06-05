
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

# Math ====
x <- mutatomic(1:10)
expect_silent(abs(x))
expect_silent(sign(x))
expect_silent(sqrt(x))
expect_silent(floor(x))
expect_silent(ceiling(x))
expect_silent(trunc(x))
expect_silent(round(x))
expect_silent(signif(x))

expect_silent(exp(x))
expect_silent(log(x))
expect_silent(expm1(x))
expect_silent(log1p(x))

x <- mutatomic(seq(0, 0.45, 0.05))
expect_silent(cos(x))
expect_silent(sin(x))
expect_silent(tan(x))
expect_silent(cospi(x))
expect_silent(sinpi(x))
expect_silent(tanpi(x))
expect_silent(acos(x))
expect_silent(asin(x))
expect_silent(atan(x))
expect_silent(cosh(x))
expect_silent(sinh(x))
expect_silent(tanh(x))
expect_silent(atanh(x))

x <- mutatomic(1:10)
expect_silent(acosh(x))
expect_silent(asinh(x))

expect_silent(lgamma(x))
expect_silent(gamma(x))
expect_silent(digamma(x))
expect_silent(trigamma(x))

expect_silent(cumsum(x))
expect_silent(cumprod(x))
expect_silent(cummax(x))
expect_silent(cummin(x))

enumerate <- enumerate + 35L


# Unary ====
x <- sample(c(TRUE, FALSE, NA), 10, replace = TRUE) |> as.mutatomic()
expect_silent(!x)

# Ops ====
# math ops:
for(xLen in c(1, 6, 10)) {
  for(yLen in c(1, 6, 10)) {
    for(xDim in c(TRUE, FALSE)) {
      for(yDim in c(TRUE, FALSE)) {
        for(xMut in c(TRUE, FALSE)) {
          for(yMut in c(TRUE, FALSE)) {
            x <- sample(1:xLen)
            y <- sample(1:yLen)
            
            if(xLen == yLen) {
              if(xDim) {
                if(xLen == 1) {
                  x.dim <- xLen
                }
                else {
                  x.dim <- c(xLen / 2, 2)
                }
                dim(x) <- x.dim
              }
              if(yDim) {
                if(yLen == 1) {
                  y.dim <- yLen
                }
                else {
                  y.dim <- c(yLen / 2, 2)
                }
                dim(y) <- y.dim
              }
              
            }
            
            if(xMut) x <- as.mutatomic(x)
            if(yMut) y <- as.mutatomic(y)
            
            if(xLen == yLen || xLen == 1L || yLen == 1L) {
              expect_silent(x + y) |> errorfun()
              expect_silent(x - y) |> errorfun()
              expect_silent(x * y) |> errorfun()
              expect_silent(x / y) |> errorfun()
              expect_silent(x ^ y) |> errorfun()
              expect_silent(x %% y) |> errorfun()
              expect_silent(x %/% y) |> errorfun()
              
              expect_silent(x == y) |> errorfun()
              expect_silent(x != y) |> errorfun()
              expect_silent(x < y) |> errorfun()
              expect_silent(x > y) |> errorfun()
              expect_silent(x <= y) |> errorfun()
              expect_silent(x >= y) |> errorfun()
            }
            else {
              expect_warning(x + y) |> errorfun()
              expect_warning(x - y) |> errorfun()
              expect_warning(x * y) |> errorfun()
              expect_warning(x / y) |> errorfun()
              expect_warning(x ^ y) |> errorfun()
              expect_warning(x %% y) |> errorfun()
              expect_warning(x %/% y) |> errorfun()
              
              expect_warning(x == y) |> errorfun()
              expect_warning(x != y) |> errorfun()
              expect_warning(x < y) |> errorfun()
              expect_warning(x > y) |> errorfun()
              expect_warning(x <= y) |> errorfun()
              expect_warning(x >= y) |> errorfun()
            }
            
            enumerate <- enumerate + 13L
            
          }
        }
        
      }
    }
  }
}




