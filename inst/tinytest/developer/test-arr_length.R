
.arr_length <- squarebrackets:::.arr_length



# various length no duplicates ====
x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
rownames(x) <- c(letters[1:10])
foo <- sb_x.array(x, sub, dims)
length(foo)

ndims <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndims)
for(i in seq_along(dims)) {
  lst[[dims[i]]] <- ci_margin(
    x, sub[[i]], dims[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, dims)
)


# various length with duplicates ====
x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
sub <- list(c("a", "a"), c(1, 1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, sub, dims)
length(foo)

ndims <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndims)
for(i in seq_along(dims)) {
  lst[[dims[i]]] <- ci_margin(
    x, sub[[i]], dims[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, dims)
)

# empty numeric ====
sub <- list(c("a", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, sub, dims)
length(foo)

ndims <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndims)
for(i in seq_along(dims)) {
  lst[[dims[i]]] <- ci_margin(
    x, sub[[i]], dims[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, dims)
)


# only FALSE ====
sub <- list(c("a", "a"), c(1, 1:4), rep(FALSE, 10))
dims <- c(1,2,4)
rownames(x) <- c(letters[1:8], "a", NA)
foo <- sb_x.array(x, sub, dims)
length(foo)

ndims <- length(dim(x))
lst <- rep(list(base::quote(expr = )), ndims)
for(i in seq_along(dims)) {
  lst[[dims[i]]] <- ci_margin(
    x, sub[[i]], dims[i]
  )
}
expect_equal(
  length(foo),
  .arr_length(x, lst, dims)
)

enumerate <- 4

