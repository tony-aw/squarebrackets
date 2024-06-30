


indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    NULL,
    logical(0),
    rep(TRUE, dim.n),
    rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1,
    1:3,
    3:1,
    c(2, 3, 1),
    1 - 1i,
    1:3 - 1i,
    3:1 - 1i,
    c(2, 3, 1) - 1i,
    1 + 1i,
    1:3 + 1i,
    3:1 + 1i,
    c(2, 3, 1) + 1i
  )
  if(test_allow_duplicates) {
    out <- c(out, list(c(1, 1, 1)))
  }
  return(out)
}

indx_named <- function(x, dim.i) {
  # note: duplicate names tested separetely, since it's something inconsistent in R
  return(c(indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))))
}


# uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col)


# unnamed matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col)


# non-uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "", NA)
colnames(x) <- c("a", "a", "", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, row = c("a", "a", "a")),
    rep3.bind(x[which(rownames(x) %in% "a"), ], 1)
  ) |> errorfun()
  expect_equal(
    sb_x(x, col = c("a", "a", "a")),
    rep3.bind(x[, which(colnames(x) %in% "a")], 2)
  ) |> errorfun()
  expect_equal(
    sb_x.array(x, c("a", "a", "a"), 1),
    rep3.bind(x[which(rownames(x) %in% "a"), ], 1)
  ) |> errorfun()
  expect_equal(
    sb_x.array(x, c("a", "a", "a"), 2),
    rep3.bind(x[, which(colnames(x) %in% "a")], 2)
  ) |> errorfun()
}


indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    logical(0),
    rep(TRUE, dim.n),
    rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1,
    1:3,
    3:1,
    c(2, 3, 1)
  )
  if(test_allow_duplicates) {
    out <- c(out, list(c(1, 1, 1)))
  }
  return(out)
}

indx_named <- function(x, dim.i) {
  # note: duplicate names tested separetely, since it's something inconsistent in R
  return(c(indx_general(x, dim.i), list("a", c("a", "b"), c("b", "a"))))
}

# uniquely named 1d array ====
x <- array(as.double(-sample.int(20)), 20)
dimnames(x) <- list(letters[1:20])
row <- indx_named(x, 1)
temp.fun.1d(x, row)


# unnamed 1d array ====
x <- array(as.double(-sample.int(20)), 20)
row <- indx_general(x, 1)
temp.fun.1d(x, row)


# non-uniquely named 1d array ====
x <- array(as.double(-sample.int(20)), 20)
dimnames(x) <- list(c("a", letters[1:17], "", NA))
row <- indx_named(x, 1)
temp.fun.1d(x, row)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, c("a", "a", "a"), 1),
    rep3.bind(x[which(rownames(x) %in% "a"), drop = FALSE], 1)
  ) |> errorfun()
}


# arbitrary dimensions ====

x <- array(as.double(seq_len(10^4)), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

sub <- list(c("a", "b"), 1:3, c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()

sub <- list(c("a", "b"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()

sub <- list(c("a", "b"), 1:4, rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()
enumerate <- enumerate + 3


if(isTRUE(test_allow_duplicates)) {
  sub <- list(c("a", "a"), c(1, 1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
  
  sub <- list(c("a", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
  
  sub <- list(c("a", "a"), c(1, 1:4), rep(FALSE, 10))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    temp.fun.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
}
enumerate <- enumerate + 3

