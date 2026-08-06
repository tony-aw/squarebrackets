
# set-up ====
enumerate <- 0L

errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}




# check special symbols ====
`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, rp = list(-10L))
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)


`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

enumerate <- enumerate + 16L


# check overwriting function internal variables ====
# new.dim and new.dimnames are the variable names used inside `%orientbc<-%`
# here we test if variables with the same names in the caller environment interfere at all
x_expr <- sample(letters)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, rp = list(-10L))
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ii_mod(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  ss_mod(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> data.frame()
expect_silent(
  tt_mod(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)


enumerate <- enumerate + 16L


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

`TRUE` <- matrix(as.list(1:20), 5, 4)
x <- `TRUE`
x[1L] <- -10L
expect_equal(
  temp_ii(`TRUE`, 1L, rp = list(-10L)),
  x
)
expect_equal(
  temp_ii(`TRUE`, 1L, tf = \(x) -10L),
  x
)

x <- `TRUE`
x[1:2, 1:2] <- -10L
expect_equal(
  temp_ss(`TRUE`, 1:2, rp = list(-10L)),
  x
)
expect_equal(
  temp_ss(`TRUE`, 1:2, tf = \(x) -10L),
  x
)

x <- `TRUE`
x[1:2, 1:2] <- -10L
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, rp = list(-10L)),
  x
)
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L),
  x
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
x <- `TRUE`
x[1:2, 1:2] <- -10L
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, rp = list(-10L)),
  x
)
expect_equal(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L),
  x
)


enumerate <- enumerate + 8L



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

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_ii(`TRUE`, 1L, rp = list(-10L))
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_ss(`TRUE`, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, rp = list(-10L))
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)


`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_ii(`TRUE`, 1L, tf = \(x) -10L)
)
expect_true(
  `TRUE`[1L] == -10L
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_ss(`TRUE`, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4)
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

`TRUE` <- matrix(as.list(1:20), 5, 4) |> as.data.frame()
expect_silent(
  temp_tt(`TRUE`, 1:2, 1:2, tf = \(x) -10L)
)
expect_true(
  all(`TRUE`[1:2, 1:2] == -10L)
)

enumerate <- enumerate + 16L


