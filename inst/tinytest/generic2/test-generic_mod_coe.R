
# set-up ====

enumerate <- 0 # to count number of tests in loops
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

obj1 <- obj2 <- data.frame(
  a = seq(1.5, 10.5, by = 1),
  b = letters[1:10],
  c = factor(letters[1:10])
)

obj2[1:5, "a"] <- letters[1:5] 

expect_equal(
  sb2_mod(obj1, 1:5, "a", rp = n(letters[1:5]), coe = TRUE),
  obj2
)

obj1 <- obj2 <- data.frame(
  a = 1:10,
  b = letters[1:10],
  c = factor(letters[1:10])
)
obj2[1:5, "a"] <- sqrt(1:5)
expect_equal(
  sb2_mod(obj1, 1:5, "a", rp = n(sqrt(1:5)), coe = as.numeric),
  obj2
)


enumerate <- enumerate + 2

