
# set-up ====

x <- array(1:prod(6:4), 6:4)
dimnames(x) <- list(
  letters[1:6],
  LETTERS[1:5],
  month.abb[1:4]
) |> errorfun()

# numeric:
expect_equal(
  sb_test(x, n(1:4, 1:2), c(-1, -3)),
  sb_test(x, n(5:6, 3:4), c(1, 3))
) |> errorfun()

# character:
expect_equal(
  sb_test(x, n(letters[1:4], month.abb[1:2]), c(-1, -3)),
  sb_test(x, n(letters[5:6], month.abb[3:4]), c(1, 3))
) |> errorfun()

# logical:
s <- lapply(dim(x), \(i)sample(c(TRUE, FALSE), i, TRUE))[c(1, 3)]
s2 <- lapply(s, \(x)!x)
expect_equal(
  sb_test(x, s, c(-1, -3)),
  sb_test(x, s2, c(1, 3))
) |> errorfun()

# missing args:
expect_equal(
  sb_test(x, use = Inf),
  sb_test(x, use = -Inf)
) |> errorfun()

# empty selection:
s <- lapply(dim(x), \(x)integer(0L))
expect_equal(
  sb_test(x, s, -1:-ndim(x)),
  x
)
expect_equal(
  sb_test(x, integer(0L), -1:-ndim(x)),
  x
) |> errorfun()
