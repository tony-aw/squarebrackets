
# set-up ====

sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())
enumerate <- 0


# array ====
x <- array(rnorm(100), c(5:3))
expect_equal(
  sbt_icom(x, slice = 1:2, use = 1),
  1:2
)
expect_equal(
  sbt_icom(x, slice = 1:2 , use = 2),
  1:2
)
expect_equal(
  sbt_icom(x, slice = 1:2 , use = 3),
  1:2
)
enumerate <- enumerate + 3L


# data.frame ====
x <- data.frame(a = 1:12, b = month.abb, c = 1:12 )
expect_equal(
  sbt_icom(x, 1:3 , 1),
  1:3
)
expect_equal(
  sbt_icom(x, 1:2 , 2),
  1:2
)
enumerate <- enumerate + 2L



