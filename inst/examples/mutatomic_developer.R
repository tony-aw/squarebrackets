

testfun1 <- function(x) {
  .internal_set_ma(x)
}


x <- 1:10
is.mutatomic(x)

testfun1(x)
is.mutatomic(x)
print(x)


