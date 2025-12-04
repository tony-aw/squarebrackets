
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


# C_any_neg ====
expect_false(
  squarebrackets:::.any_neg(c(0L, 10L))
)
expect_true(
  squarebrackets:::.any_neg(rep(-10L, 10L))
)
enumerate <- enumerate + 2


# C_any_nonpos ====
expect_false(
  squarebrackets:::.any_nonpos(rep(20L, 10L))
)
expect_true(
  squarebrackets:::.any_nonpos(rep(-10L, 10L))
)
expect_true(
  squarebrackets:::.any_nonpos(rep(0L, 10L))
)
enumerate <- enumerate + 3


# C_any_badindx ====
expect_false( # false
  squarebrackets:::.any_badindx(1:10L, 20L) 
)
expect_true( # true because zero
  squarebrackets:::.any_badindx(0:10, 11L) 
)
expect_true( # true because negative
  squarebrackets:::.any_badindx(-1:-10, 11L) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badindx(rep(20L, 10L), 11L) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badindx(rep(-20L, 10L), 11L)
)
enumerate <- enumerate + 5



# C_any_badmargin ====
expect_false( # false
  squarebrackets:::.any_badmargin(0:10, 20L) 
)
expect_true( # true because negative
  squarebrackets:::.any_badmargin(-1:10, 11L) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badmargin(rep(20L, 10), 11L) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badmargin(rep(-20L, 10L), 11L)
)
enumerate <- enumerate + 4



# C_ss2ii_d ====
for(i in 2:7) {
  m <- (2^31 + 10)^(1/i) |> ceiling()
  sub <- rep(list(m), i) |> lapply(as.integer)
  x.dim <- rep(m, i)
  args <- c(sub, n(x.dim))
  expect_equal(
    squarebrackets:::.ss2ii_d64(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# rcpp_ss2ii_general ====
for(i in 2:8) {
  m <- ceiling( (100)^(1/i) )
  sub <- rep(list(m), i)
  x.dim <- rep(m, i)
  dimcumprod <- cumprod(x.dim)
  args <- c(sub, n(dimcumprod))
  
  expect_equal(
    squarebrackets:::.ss2ii_general64(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# ss2ii ====
for(i in 2:8) {
  m <- ceiling( (100)^(1/i) )
  sub <- rep(list(m), i)
  x.dim <- rep(m, i)
  dimcumprod <- cumprod(x.dim)
  args <- c(sub, n(dimcumprod))
  
  expect_equal(
    ss2ii(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}



# C_convert_cplx ====
expect_equal(
  squarebrackets:::.C_convert_cplx(20 * -1i, 20L),
  1L
)
expect_equal(
  squarebrackets:::.C_convert_cplx(1i, 20L),
  1L
)
expect_equal(
  squarebrackets:::.C_convert_cplx(20 * 1i, 20L),
  20L
)
expect_equal(
  squarebrackets:::.C_convert_cplx(-1i, 20L),
  20L
)
enumerate <- enumerate + 4
