
# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}



# check special symbols ====
`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, rp = -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

enumerate <- enumerate + 12L


# check overwriting function internal variables ====
# new.dim and new.dimnames are the variable names used inside `%orientbc<-%`
# here we test if variables with the same names in the caller environment interfere at all
x_expr <- sample(letters)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, rp = -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

enumerate <- enumerate + 12L


# check as returning function ====
temp_ii <- function(x, i, rp, tf) {
  ii_mod(x, i, rp = rp, tf = tf)
  return(x)
}
temp_ss <- function(x, s, use = Inf, rp, tf) {
  ss_mod(x, s, use, rp = rp, tf = tf)
  return(x)
}
temp_tt <- function(x, row, col, use = 1:2, rp, tf) {
  tt_mod(x, row, col, use, rp = rp, tf = tf)
  return(x)
}

`TRUE` <- matrix(1:20, 5, 4)
x <- `TRUE`
x[1L] <- -10L
expect_equal(
  temp_ii(`TRUE`, 1L, rp = -10L),
  x
)
expect_equal(
  temp_ii(`TRUE`, 1L, tf = \(x) -10L),
  x
)

x <- `TRUE`
x[1:2, 1:2] <- -10L
expect_equal(
  temp_ss(`TRUE`, 1:2, rp = -10L),
  x
)
expect_equal(
  temp_ss(`TRUE`, 1:2, tf = \(x) -10L),
  x
)

x <- `TRUE`
x[1:2, 1:2] <- -10L
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, rp = -10L),
  x
)
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L),
  x
)

enumerate <- enumerate + 12L



# check as passing function ====
temp_ii <- function(x, i, rp, tf) {
  env <- parent.frame()
  eval(substitute(ii_mod(x, i, rp = rp, tf = tf)), envir = env)
}
temp_ss <- function(x, s, use = Inf, rp, tf) {
  env <- parent.frame()
  eval(substitute(ss_mod(x, s, use, rp = rp, tf = tf)), envir = env)
  return(x)
}
temp_tt <- function(x, row, col, use = 1:2, rp, tf) {
  env <- parent.frame()
  eval(substitute(tt_mod(x, row, col, use, rp = rp, tf = tf)), envir = env)
  return(x)
}

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_ii(`TRUE`, 1L, rp = -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_ss(`TRUE`, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, rp = -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_ii(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_ss(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(1:20, 5, 4)
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

enumerate <- enumerate + 12L



# check no unnecessary copies ====
.rcpp_address <- broadcast:::.rcpp_address
y <- matrix(rnorm(1e6), 1e4, 100)
ii_mod(y, 1, rp = -1)
address1 <- .rcpp_address(y) # get address after first change
ii_mod(y, 2, rp = -2)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)

ss_mod(y, 1, rp = -1)
address1 <- .rcpp_address(y) # get address after first change
ss_mod(y, 2, rp = -2)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)

tt_mod(y, 2, 2, rp = -1)
address1 <- .rcpp_address(y) # get address after first change
tt_mod(y, 3, 3, rp = -2)
address2 <- .rcpp_address(y)
expect_equal(
  address1, address2
)

enumerate <- enumerate + 3L


# check large indices are not secretly implemented symbolically and explode memory ====
y <- matrix(sample(as.raw(0:255), 5000^2, TRUE), 5000, 5000) 

expect_silent(
  ii_mod(y, sample(1:length(y)), tf = \(x) as.raw(0))
)
expect_silent(
  ss_mod(y, n(sample(1:5000), sample(1:5000)), tf = \(x) as.raw(0))
)
expect_silent(
  tt_mod(y, sample(1:5000), sample(1:5000), tf = \(x) as.raw(0))
)

enumerate <- enumerate + 3L

