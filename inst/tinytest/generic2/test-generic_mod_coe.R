
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# increase numeric-like complexity ====

obj1 <- obj2 <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE), # logical
  b = 1:10, # integer
  c = seq(1.5, 10.5, by = 1), # double
  d = sample(1.5:10.5) + sample(1.5:10.5) * 1i
)
obj2[1:5, "a"] <- 1:5
expect_equal(
  sb2_mod(obj1, 1:5, "a", rp = 1:5),
  obj2
)

obj1 <- obj2 <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE), # logical
  b = 1:10, # integer
  c = seq(1.5, 10.5, by = 1), # double
  d = sample(1.5:10.5) + sample(1.5:10.5) * 1i
)
obj2[1:5, "b"] <- sqrt(1:5)
expect_equal(
  sb2_mod(obj1, 1:5, "b", rp = sqrt(1:5)),
  obj2
)

obj1 <- obj2 <- data.frame(
  a = sample(c(TRUE, FALSE, NA), 10, TRUE), # logical
  b = 1:10, # integer
  c = seq(1.5, 10.5, by = 1), # double
  d = sample(1.5:10.5) + sample(1.5:10.5) * 1i
)
obj2[1:5, "c"] <- obj2$d[5:1]
expect_equal(
  sb2_mod(obj1, 1:5, "c", rp = obj2$d[5:1]),
  obj2
)
enumerate <- enumerate + 3L


# factors ====
obj1 <- obj2 <- data.frame(
  a = 1:10,
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj2[1:5, "c"] <- 1:5
expect_equal(
  sb2_mod(obj1, 1:5, "c", rp = 1:5),
  obj2
)


enumerate <- enumerate + 1L

