# set-up ====
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L
n <- 10
use <- c(1, -1)

tempfun1 <- function(x, v, use) {
  
  check <- ifelse(is.na(x), TRUE, x == v)
  if(use < 0) check <- !check
  return(sum(check))
}

tempfun2 <- function(x, v, use) {
  
  check <- x >= v[1] & x <= v[2]
  if(use < 0) {
    check <- !check
  }
  check[is.na(x)] <- TRUE
  
  return(sum(check))
}



tempfun_multistring <- function(x, v, use) {
  
  check <- x %in% v
  if(use < 0) {
    check <- !check
  }
  check[is.na(x)] <- TRUE
  return(sum(check))
}




# logical, complex, raw ====
x <- sample(c(TRUE, FALSE, NA), n, TRUE)
v <- c(TRUE, FALSE)
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in v) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, iV, iUse)
      out[[counter]] <- countv(x, v = iV, na = TRUE, use = iUse)
      
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter

samp1 <- c(rnorm(n - 4), NA, NaN, Inf -Inf, 1)
samp2 <- c(rnorm(n - 4), NA, NaN, Inf -Inf, 1)
x <- samp1 + samp2 * - 1i
v <- 1-1i
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in v) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, iV, iUse)
      out[[counter]] <- countv(x, v = iV, na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter

x <- as.raw(sample(1:10))
v <- c(x[1], x[2])
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in v) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, iV, iUse)
      out[[counter]] <- countv(x, v = iV, na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter



# integer, single ====
x <- sample(c(1:5, NA), n, TRUE)
v <- n(2, 3, 2.0, 3.0)
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# integer, range ====
x <- sample(c(1:5, NA), n, TRUE)
v <- n(as.integer(2:3), as.double(2.0:3.0))
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun2(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter



# double, single ====
x <- sample(c(1.0:5.0, NA, NaN, Inf, -Inf), n, TRUE)
v <- n(2, 3, 2.0, 3.0)
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# double, range ====
x <- sample(c(1.0:5.0, NA, NaN, Inf, -Inf), n, TRUE)
v <- n(as.integer(2:3), as.double(2.0:3.0))
expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun2(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# string, single ====
x <- sample(c(letters[1:5], "回收站", NA), 10, TRUE)
v <- c("a", "z", "回收站")

expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter




# string, multiple (including multiple encodings) ====
x <- sample(c(letters[1:5], "回收站", NA), 10, TRUE)
v1 <- c("a", "回收站")
v2 <- c("z", "回收站")
Encoding(v1) <- c("UTF-8")
Encoding(v2) <- "unknown"
v <- c(v1, v2)
Encoding(v)

expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun_multistring(x, v, iUse)
      out[[counter]] <- countv(x, v = v, na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter




# factouse ====
x <- factor(sample(letters, 10, TRUE))
v <- sample(x, 2)

expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter




# factor, v = string ====
x <- factor(sample(letters, 10, TRUE))
v <- sample(as.character(x), 2L)

expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(x, v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# factor, v = integeuse ====
x <- factor(sample(letters, 10, TRUE))
v <- 1:2

expected <- out <- vector("list", length(v) * length(use) )
counter <- 1L
for(iV in seq_along(v)) {
  for(iUse in use) {
      expected[[counter]] <- tempfun1(unclass(x), v[[iV]], iUse)
      out[[counter]] <- countv(x, v = v[[iV]], na = TRUE, use = iUse)
      counter <- counter + 1L
  }
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter





