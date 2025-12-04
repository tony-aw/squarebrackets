
enumerate <- 0

# lst_rec ====
x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  )
)
expect_equal(
  lst_rec(x, c(1,2,2)),
  "ABB"
)
expect_equal(
  lst_rec(x, c(2,2,1)),
  "BBA"
)

expect_equal(
  lst_rec(x, 1:2),
  x[[1:2]]
)

x2 <- x
x2[[1:2]] <- "foo"
lst_recin(x, 1:2, rp = "foo")
expect_equal(
  x,
  x2
)

x2[[2:1]] <- stringi::stri_reverse(x2[[2:1]])
lst_recin(x, 2:1, tf = stringi::stri_reverse)
expect_equal(
  x,
  x2
)

enumerate <- enumerate + 5


# error checks ====
expect_error(
  lst_rec(x, NA),
  pattern = "`rec` cannot contain `NA`"
)
expect_error(
  lst_rec(x, c("a", NA)),
  pattern = "`rec` cannot contain `NA`"
)
expect_error(
  lst_rec(x, c(1L, NA)),
  pattern = "`rec` cannot contain `NA`"
)
expect_error(
  lst_rec(x, TRUE),
  pattern = "`rec` must be an integer vector or a character vector"
)

print(enumerate)


