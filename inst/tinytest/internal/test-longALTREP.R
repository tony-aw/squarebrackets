
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

n <- 2L^31L + 10L

# C_any_neg ====
x <- 0:n
expect_true(
  squarebrackets:::.C_is_altrep(x)
)
expect_false(
  squarebrackets:::.any_neg(x)
)
x <- -1:n
expect_true(
  squarebrackets:::.any_neg(x)
)
enumerate <- enumerate + 2


# C_any_nonpos ====
x <- 1:n
expect_false(
  squarebrackets:::.any_nonpos(x)
)
x <- n:0
expect_true(
  squarebrackets:::.any_nonpos(x)
)
x <- 1:-n
expect_true(
  squarebrackets:::.any_nonpos(x)
)
enumerate <- enumerate + 3


# C_any_badindx ====
expect_false( # false
  squarebrackets:::.any_badindx(1:n, 2^31 + 10) 
)
expect_true( # true because zero
  squarebrackets:::.any_badindx(n:0, 11) 
)
expect_true( # true because negative
  squarebrackets:::.any_badindx(-1:-n, 11) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badindx(1:n, 11) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badindx(-n:n, 11)
)
enumerate <- enumerate + 5



# C_any_badmargin ====
expect_false( # false
  squarebrackets:::.any_badmargin(0:n, 2^31 + 10) 
)
expect_true( # true because negative
  squarebrackets:::.any_badmargin(-1:n, 11) 
)
expect_true( # true because out of bounds
  squarebrackets:::.any_badmargin(1:n, 11) 
)
expect_true( # true because negative AND out of bounds
  squarebrackets:::.any_badmargin(-n:n, 11)
)
enumerate <- enumerate + 4


