
enumerate <- 0

# seq_rec ====

expect_equal( # Fibonacci numbers
  seq_rec(),
  c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
)

expect_equal( # Lucas numbers
  seq_rec(inits = c(2,1)),
  c(2, 1, 3, 4, 7, 11, 18, 29, 47, 76)
)

expect_equal( # Pell numbers
  seq_rec(f=\(x)2*x[2]+x[1]),
  c(0, 1, 2, 5, 12, 29, 70, 169, 408, 985)
)

expect_equal(
  seq_rec(inits = c(1,0), f=\(x)2*x[1]),
  c(1, 0, 2, 0, 4, 0, 8, 0, 16, 0)
)

expect_equal( # Jacobsthal numbers
  seq_rec(f=\(x)x[2]+2*x[1]),
  c(0, 1, 1, 3, 5, 11, 21, 43, 85, 171)
)

expect_equal( # Padovan sequence
  seq_rec(c(1,1,1), f=\(x)x[1] + x[2]),
  c(1, 1, 1, 2, 2, 3, 4, 5, 7, 9)
)

expect_equal( # Perrin numbers
  seq_rec(c(3,0,2), f=\(x)x[1] + x[2]),
  c(3, 0, 2, 3, 2, 5, 5, 7, 10, 12)
)

expect_equal( # Triangular numbers
  seq_rec(c(0,1,3), f=\(x)3*x[3] - 3*x[2] + x[1]),
  c(0, 1, 3, 6, 10, 15, 21, 28, 36, 45)
)

enumerate <- enumerate + 8


# seq_mlen ====
expect_equal(
  seq_mlen(1:10),
  lapply(1:10, seq_len)
)
expect_error(
  seq_mlen(letters)
)
enumerate <- enumerate + 2


# seq_names ====
expect_error(
  seq_names(character(0), "a", "b"),
  pattern = "no names given"
)
expect_error(
  seq_names(c("", letters), "a", "b"),
  pattern = "empty names not allowed"
)
expect_error(
  seq_names(letters, "aa", "b"),
  pattern = "`start` not in `names`",
  fixed = TRUE
)
expect_error(
  seq_names(letters, "a", "bb"),
  pattern = "`end` not in `names`",
  fixed = TRUE
)
expect_error(
  seq_names(letters, "a", "y", inv = NA),
  pattern = "`inv` must be `TRUE` or `FALSE`",
  fixed = TRUE
)
expect_equal(
  seq_names(rev(letters), "a", "y"),
  26:2
)
expect_equal(
  seq_names(letters, "y", "a"),
  25:1
)
enumerate <- enumerate + 7


