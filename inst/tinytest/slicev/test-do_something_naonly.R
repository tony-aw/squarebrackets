# set-up ====
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

enumerate <- 0L
n <- 10
rel <- c(TRUE, FALSE)

tempfun1 <- function(x, rel) {
  
  if(!rel) {
    return(sum(!is.na(x)))
  }
  else {
    return(sum(is.na(x)))
  }
}


# logical ====
x <- sample(c(TRUE, FALSE, NA), n, TRUE)
expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# complex ====
samp1 <- c(rnorm(n - 4), NA, NaN, Inf -Inf, 1)
samp2 <- c(rnorm(n - 4), NA, NaN, Inf -Inf, 1)
x <- samp1 + samp2 * - 1i
expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# integer ====
x <- sample(c(1:5, NA), n, TRUE)
expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# double ====
x <- sample(c(1.0:5.0, NA, NaN, Inf, -Inf), n, TRUE)
expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter



# string ====
x <- sample(c(letters[1:5], "回收站", NA), 10, TRUE)
v <- c("a", "z", "回收站")

expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter




# factor ====
x <- factor(sample(c(letters[1:5], NA), 10, TRUE))

expected <- out <- vector("list", length(rel))
counter <- 1L
for(iInv in rel) {

    expected[[counter]] <- tempfun1(x, iInv)
    out[[counter]] <- countv(x, na = NA, r = iInv)
}
expect_equal(
  expected, out
)
enumerate <- enumerate + counter


