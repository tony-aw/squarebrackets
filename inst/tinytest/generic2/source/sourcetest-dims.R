
# set-up ====

af <- function(x) {
  return(as.list(as.double(x)))
}


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


temp.fun.2d <- function(x, row, col, f_expect, f_out) {
  out <- expected <- vector("list", length(row) * length(col))
  k <- 1
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      
      len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
      rp <- af(sample(c(seq_len(len), NA), size = len))
      
      sub <- n(row[[i]], col[[j]])
      dims <- 1:2
      rem <- which(vapply(sub, is.null, logical(1L)))
      if(length(rem) > 0L) {
        sub <- sub[-rem]
        dims <- dims[-rem]
      }
      
      expected[[k]] <- f_expect(x, row[[i]], col[[j]])
      out[[k]] <- f_out(x, sub, dims)
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
      k <- k + 1
    }
  }
  expect_equal(expected, out) |> errorfun()
  expect_true(all(sapply(out, is.array))) |> errorfun()
}


temp.fun.1d <- function(x, row, f_expect, f_out) {
  out <- expected <- vector("list", length(row))
  
  for(i in 1:length(row)) {
    
    len <- length(pre_subset_1d(x, row[[i]]))
    rp <- af(sample(c(seq_len(len), NA), size = len))
    
    expected[[i]] <- f_expect(x, row[[i]])
    out[[i]] <- f_out(x, row[[i]], 1)
    assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
  }
  expect_equal(expected, out) |> errorfun()
  expect_true(all(sapply(out, is.array))) |> errorfun()
}



# uniquely named matrix ====
x <- matrix(af(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
# temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)


# unnamed matrix ====
x <- matrix(af(-sample.int(20)), nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
# temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)


# non-uniquely named matrix ====
x <- matrix(af(-sample.int(20)), nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "", NA)
colnames(x) <- c("a", "a", "", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
# temp.fun.matrix(x, row, col)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb2_x.array(x, c("a", "a", "a"), 1),
    rep3.bind(x[which(rownames(x) %in% "a"), ], 1)
  ) |> errorfun()
  expect_equal(
    sb2_x.array(x, c("a", "a", "a"), 2),
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
x <- array(af(-sample.int(20)), 20)
dimnames(x) <- list(letters[1:20])
row <- indx_named(x, 1)
temp.fun.1d(x, row, f_expect.1d, f_out.1d)


# unnamed 1d array ====
x <- array(af(-sample.int(20)), 20)
row <- indx_general(x, 1)
temp.fun.1d(x, row, f_expect.1d, f_out.1d)


# non-uniquely named 1d array ====
x <- array(af(-sample.int(20)), 20)
dimnames(x) <- list(c("a", letters[1:17], "", NA))
row <- indx_named(x, 1)
temp.fun.1d(x, row, f_expect.1d, f_out.1d)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb2_x(x, c("a", "a", "a"), 1),
    rep3.bind(x[which(rownames(x) %in% "a"), drop = FALSE], 1)
  ) |> errorfun()
}


# arbitrary dimensions ====

x <- array(af(seq_len(10^4)), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

sub <- list(c("a", "b"), 1:3, c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()

sub <- list(c("a", "b"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()

sub <- list(c("a", "b"), 1:4, rep(FALSE, 10))
dims <- c(1,2,4)
expect_equal(
  sb_test(x, sub, dims),
  f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
) |> errorfun()
enumerate <- enumerate + 3


if(isTRUE(test_allow_duplicates)) {
  sub <- list(c("a", "a"), c(1, 1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
  
  sub <- list(c("a", "a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
  
  sub <- list(c("a", "a"), c(1, 1:4), rep(FALSE, 10))
  dims <- c(1,2,4)
  expect_equal(
    sb_test(x, sub, dims),
    f_expect.arbitrary(x, sub[[1]], sub[[2]], sub[[3]])
  ) |> errorfun()
}
enumerate <- enumerate + 3

