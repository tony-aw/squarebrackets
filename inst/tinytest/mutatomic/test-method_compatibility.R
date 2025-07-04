
# set-up ====

enumerate <- 0 # to count number of tests in loops
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

as.someclass <- function(x) {
  class(x) <- c("someclass", oldClass(x))
  return(x)
}


`[.someclass` <- function(x, ..., drop = FALSE) {
  
  y <- NextMethod("[")
  class(y) <- oldClass(x)
  y
}


`[[.someclass` <- function(x, ...) {
  
  y <- NextMethod("[[")
  class(y) <- oldClass(x)
  y
}


# subset vector ===
x <- as.someclass(1:10)
y <- x
expected <- x[1:2]
expected <- as.mutatomic(expected)
y <- as.mutatomic(y)
expect_equal(
  expected,
  y[1:2]
)


# subset matrix ===
x <- as.someclass(matrix(1:20, ncol = 4))
y <- x
expected <- x[1:2,]
expected <- as.mutatomic(expected)
y <- as.mutatomic(y)
expect_equal(
  expected,
  y[1:2,]
)


# subset2 vector ===
x <- as.someclass(1:10)
y <- x
expected <- x[[2]]
expected <- as.mutatomic(expected)
y <- as.mutatomic(y)
expect_equal(
  expected,
  y[[2]]
)


# subset2 matrix ===
x <- as.someclass(matrix(1:20, ncol = 4))
y <- x
expected <- x[[2, 3]]
expected <- as.mutatomic(expected)
y <- as.mutatomic(y)
expect_equal(
  expected,
  y[[2, 3]]
)

enumerate <- enumerate + 4L



