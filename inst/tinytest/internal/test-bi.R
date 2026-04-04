
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

.internal_bi <- squarebrackets:::.internal_bi


# scalar ====
expect_equal(
  .internal_bi(1, 10),
  1
)
expect_equal(
  .internal_bi(-10, 10),
  1
)


# regular vector ====
ind <- sample(c(1:10, -1:-10))
expected <- ifelse(ind < 0, 10 + ind + 1, ind)
expect_equal(
  .internal_bi(list(ind), 10),
  expected
)

# ALTREP vector ====
ind <- -10:-1
expected <- 10 + ind + 1
expect_equal(
  .internal_bi(list(ind), 10),
  expected
)


# multiple vectors ====
ind1 <- sample(1:10)
ind2 <- sample(-10:-1)
expected <- c(ind1, 10 + ind2 + 1)
expect_equal(
  .internal_bi(list(ind1, ind2), 10),
  expected
)

# multiple ALTREP ====
ind1 <- 10:1
ind2 <- -10:-1
expected <- c(ind1, 10 + ind2 + 1)
expect_equal(
  .internal_bi(list(ind1, ind2), 10),
  expected
)

