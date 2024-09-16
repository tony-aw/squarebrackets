
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))


# C_any_neg ====
expect_false(
  squarebrackets:::.any_neg(c(0, 2^31 + 10))
)
expect_true(
  squarebrackets:::.any_neg(rep(-2^31 - 10, 10))
)
enumerate <- enumerate + 2


# C_any_nonpos ====
expect_false(
  squarebrackets:::.any_nonpos(rep(2^31 + 10, 10))
)
expect_true(
  squarebrackets:::.any_nonpos(rep(-2^31 - 10, 10))
)
expect_true(
  squarebrackets:::.any_nonpos(rep(0, 10))
)
enumerate <- enumerate + 3


# C_any_badindx ====
expect_false( # false
  squarebrackets:::.any_badindx(1:10, 2^31 + 10) 
)
expect_true( # true because zero
  squarebrackets:::.any_badindx(0:10, 11) 
)
expect_true( # true because negative
  squarebrackets:::.any_badindx(-1:-10, 11) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badindx(rep(2^31 + 10, 10), 11) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badindx(rep(-2^31 - 10, 10), 11)
)
enumerate <- enumerate + 5



# C_any_badmargin ====
expect_false( # false
  squarebrackets:::.any_badmargin(0:10, 2^31 + 10) 
)
expect_true( # true because negative
  squarebrackets:::.any_badmargin(-1:10, 11) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badmargin(rep(2^31 + 10, 10), 11) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badmargin(rep(-2^31 - 10, 10), 11)
)
enumerate <- enumerate + 4



# C_sub2ind_dims ====
for(i in 2:7) {
  m <- (2^31 + 10)^(1/i) |> ceiling()
  sub <- rep(list(m), i) |> lapply(as.integer)
  x.dim <- rep(m, i)
  args <- c(sub, n(x.dim))
  expect_equal(
    squarebrackets:::.sub2ind_d64(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# rcpp_sub2ind_general ====
for(i in 2:8) {
  m <- ceiling( (2^31 + 10)^(1/i) )
  sub <- rep(list(m), i)
  x.dim <- rep(m, i)
  dimcumprod <- cumprod(x.dim)
  args <- c(sub, n(dimcumprod))
  
  expect_equal(
    squarebrackets:::.sub2ind_general64(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# sub2ind ====
for(i in 2:8) {
  m <- ceiling( (2^31 + 10)^(1/i) )
  sub <- rep(list(m), i)
  x.dim <- rep(m, i)
  dimcumprod <- cumprod(x.dim)
  args <- c(sub, n(dimcumprod))
  
  expect_equal(
    sub2ind(sub, x.dim),
    m^i
  ) |> errorfun()
  enumerate <- enumerate + 1
}
