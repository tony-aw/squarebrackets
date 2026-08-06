
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

d <- data.frame(a = 1:10, b = letters[1:10], c = rnorm(10), d = month.abb[1:10])

expect_equal(
  collapse::ss(d, with(d, a > 5 & b != "j"), c(2L, 4L)),
  tt_x(d, ~~ a > 5 & b != "j", is.numeric, -2L)
)
enumerate <- enumerate + 1L