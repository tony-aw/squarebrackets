
# set-up ====

indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  out <- list(
    NULL,
    0L,
    logical(0),
    rep(TRUE, dim.n),
    rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1,
    1:3,
    3:1,
    c(2, 3, 1),
    1 * -1 |> bi(),
    1:3 * -1 |> bi(),
    3:1 * -1 |> bi(),
    c(2, 3, 1) * -1 |> bi(),
    1 |> bi(),
    1:3 |> bi(),
    3:1 |> bi(),
    c(2, 3, 1) |> bi()
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
      rp <- sample(c(seq_len(len), NA), size = len)
      
      expected[[k]] <- f_expect(x, row[[i]], col[[j]])
      out[[k]] <- f_out(x, row[[i]], col[[j]])
      assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
      k <- k + 1
    }
  }
  expect_equal(expected, out) |> errorfun()
  expect_true(all(sapply(out, is.array))) |> errorfun()
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

# uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), 5, 4)
rownames(x) <- letters[1:5]
colnames(x) <- LETTERS[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)


# unnamed matrix ====
x <- matrix(as.double(-sample.int(20)), c(5, 4))
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)


# non-uniquely named matrix ====
x <- matrix(as.double(-sample.int(20)), c(5, 4))
rownames(x) <- c("a", letters[1:2], "", NA)
colnames(x) <- LETTERS[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.2d(x, row, col, f_expect.2d, f_out.2d)

if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sbt_x(x, c("a", "a", "a"), 0L),
    rep3.bind(x[which(rownames(x) %in% "a"), , drop = FALSE], 1)
  ) |> errorfun()
}

