


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


# uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)


# unnamed matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.matrix(x, row, col)


# non-uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "c", NA)
colnames(x) <- c("a", "a", "b", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, row = c("a", "a", "a")),
    rep3.bind(x[which(rownames(x) %in% "a"), ], 1)
  ) |> errorfun()
}
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, col = c("a", "a", "a")),
    rep3.bind(x[, which(colnames(x) %in% "a")], 2)
  ) |> errorfun()
}


# uniquely named 3darray ====
x.dim <- c(6, 5, 4)
x.flat <- -sample.int(prod(x.dim))
x <- array(as.double(x.flat), dim = x.dim)
dimnames(x) <- list(letters[1:6], letters[1:5], letters[1:4])
row <- indx_named(x, 1)
col <- indx_named(x, 2)
lyr <- indx_named(x, 3)
temp.fun.3darray(x, row, col, lyr)


# unnamed 3darray ====
x.dim <- c(6, 5, 4)
x.flat <- -sample.int(prod(x.dim))
x <- array(as.double(x.flat), dim = x.dim)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
lyr <- indx_general(x, 3)
temp.fun.3darray(x, row, col, lyr)


# non-uniquely named 3darray ====
x.dim <- c(6, 5, 4)
x.flat <- -sample.int(prod(x.dim))
x <- array(as.double(x.flat), dim = x.dim)
dimnames(x) <- list(c("a", "a", "b", "c", "d", NA), c("a", "a", "b", "c", NA), c("a", "a", "b", NA))
row <- indx_named(x, 1)
col <- indx_named(x, 2)
lyr <- indx_named(x, 3)
temp.fun.3darray(x, row, col, lyr)

if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, rcl = list(c("a", "a", "a"), NULL, NULL)),
    rep3.bind(x[which(rownames(x) %in% "a"), , ], 1)
  ) |> errorfun()
}
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, rcl = list(NULL, c("a", "a", "a"), NULL)),
    rep3.bind(x[, which(colnames(x) %in% "a"), ], 2)
  ) |> errorfun()
}
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, rcl = list(NULL, NULL, c("a", "a", "a"))),
    rep3.bind(x[, , which(dimnames(x)[[3]] %in% "a")], 3)
  ) |> errorfun()
}
