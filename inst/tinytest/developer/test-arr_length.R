
.arr_length <- squarebrackets:::.arr_length



# various length no duplicates ====
x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
s <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
rownames(x) <- c(letters[1:10])
foo <- sb_x.array(x, s, d)
length(foo)

ndim <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndim)
for(i in seq_along(d)) {
  lst[[d[i]]] <- ci_margin(
    x, s[[i]], d[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, d)
)


# various length with duplicates ====
x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
s <- list(c("a", "a"), c(1, 1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, s, d)
length(foo)

ndim <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndim)
for(i in seq_along(d)) {
  lst[[d[i]]] <- ci_margin(
    x, s[[i]], d[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, d)
)

# empty numeric ====
s <- list(c("a", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
d <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, s, d)
length(foo)

ndim <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndim)
for(i in seq_along(d)) {
  lst[[d[i]]] <- ci_margin(
    x, s[[i]], d[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, d)
)


# only FALSE ====
s <- list(c("a", "a"), c(1, 1:4), rep(FALSE, 10))
d <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, s, d)
length(foo)

ndim <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndim)
for(i in seq_along(d)) {
  lst[[d[i]]] <- ci_margin(
    x, s[[i]], d[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, d)
)

enumerate <- 4

