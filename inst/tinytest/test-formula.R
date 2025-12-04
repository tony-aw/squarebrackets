


# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# vector ====
x <- as.mutatomic(array(1:100, 100))
names(x) <- sample(c(letters, LETTERS, month.abb, month.name), 100, TRUE)

funlist <- list(
  ss_x,
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, rp = -1),
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, tf = mean),
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, rp = -1)
    return(x)
  },
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, tf = mean)
    return(x)
  }
)



for(f in funlist) {
  
  n <- length(x)
  
  # .M:
  expect_equal(
    f(x, ~ .M),
    f(x, 1)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .M, -1),
    f(x, 1, -1)
  ) |> errorfun()
  
  # .N:
  expect_equal(
    f(x, ~ .N),
    f(x, n)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .N, -1),
    f(x, n, -1)
  ) |> errorfun()
  
  # .Nms:
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms)),
    f(x, match(names(x)[1], names(x)))
  ) |> errorfun()
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms), -1),
    f(x, match(names(x)[1], names(x)), -1)
  ) |> errorfun()
  
  # .bi():
  expect_equal(
    f(x, ~ .bi(1, -1)),
    f(x, c(1, n))
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(1, -1), -1),
    f(x, c(1, n), -1)
  ) |> errorfun()
  
  # .bi() + keyword
  expect_equal(
    f(x, ~ .bi(-.I)),
    f(x, rev(seq_len(n)))
  ) |> errorfun()
  
  expect_equal(
    f(x, ~ .bi(-.I), -1),
    f(x, rev(seq_len(n)), -1)
  ) |> errorfun()
  
  enumerate <- enumerate + 10L
}


# 1d array ====
x <- as.mutatomic(array(1:100, 100))
dimnames(x) <- list(
  sample(c(letters, LETTERS, month.abb, month.name), 100, TRUE)
)

funlist <- list(
  ss_x,
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, rp = -1),
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, tf = mean),
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, rp = -1)
    return(x)
  },
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, tf = mean)
    return(x)
  }
)



for(f in funlist) {
  
  n <- dim(x)[1]
  
  # .M:
  expect_equal(
    f(x, ~ .M),
    f(x, 1)
  ) |> errorfun()
  # .M:
  expect_equal(
    f(x, ~ .M, -1),
    f(x, 1, -1)
  ) |> errorfun()
  
  # .N:
  expect_equal(
    f(x, ~ .N),
    f(x, n)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .N, -1),
    f(x, n, -1)
  ) |> errorfun()
  
  # .Nms:
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms)),
    f(x, match(dimnames(x)[[1]][1], dimnames(x)[[1]]))
  ) |> errorfun()
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms), -1),
    f(x, match(dimnames(x)[[1]][1], dimnames(x)[[1]]), -1)
  ) |> errorfun()
  
  # .bi():
  expect_equal(
    f(x, ~ .bi(1, -1)),
    f(x, c(1, n))
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(1, -1), -1),
    f(x, c(1, n), -1)
  ) |> errorfun()
  
  # .bi() + keyword
  expect_equal(
    f(x, ~ .bi(-.I)),
    f(x, rev(seq_len(n)))
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(-.I), -1),
    f(x, rev(seq_len(n)), -1)
  ) |> errorfun()
  
  enumerate <- enumerate + 10L
}



# 3d array ====
x <- as.mutatomic(array(1:prod(6:4), 6:4))
dimnames(x) <- list(
  letters[1:6],
  LETTERS[1:5],
  month.abb[1:4]
)

funlist <- list(
  ss_x,
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, rp = -1),
  \(x, ss = NULL, use = 1:ndim(x)) ss_mod(x, ss, use, chkdup = FALSE, tf = mean),
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, rp = -1)
    return(x)
  },
  \(x, ss = NULL, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, use, tf = mean)
    return(x)
  }
)



for(f in funlist) {
  n <- dim(x)[3]
  
  # .M:
  expect_equal(
    f(x, ~ .M, 3),
    f(x, 3, 3)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .M, -3),
    f(x, 3, -3)
  ) |> errorfun()
  
  # .N:
  expect_equal(
    f(x, ~ .N, 3),
    f(x, n, 3)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .N, -3),
    f(x, n, -3)
  ) |> errorfun()
  
  # .Nms:
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms), 3),
    f(x, match(dimnames(x)[[3]][1], dimnames(x)[[3]]), 3)
  ) |> errorfun()
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms), -3),
    f(x, match(dimnames(x)[[3]][1], dimnames(x)[[3]]), -3)
  ) |> errorfun()
  
  # .bi():
  expect_equal(
    f(x, ~ .bi(1, -1), 3),
    f(x, c(1, n), 3)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(1, -1), -3),
    f(x, c(1, n), -3)
  ) |> errorfun()
  
  # .bi() + keyword
  expect_equal(
    f(x, ~ .bi(-.I), 3),
    f(x, rev(seq_len(n)), 3)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(-.I), -3),
    f(x, rev(seq_len(n)), -3)
  ) |> errorfun()
  
  enumerate <- enumerate + 10L
  
}


# matrix ====

x <- as.mutatomic(matrix(sample(1:100), 10, 10))
rownames(x) <- month.abb[1:10]
colnames(x) <- month.name[1:10]
expect_equal(
  ndim(x), 2L
)

funlist <- list(
  sbt_x,
  \(x, row = NULL, col = NULL, use = 1:2) sbt_mod(x, row, col, use, chkdup = FALSE, rp = -1),
  \(x, row = NULL, col = NULL, use = 1:2) sbt_mod(x, row, col, use, chkdup = FALSE, tf = mean),
  \(x, row = NULL, col = NULL, use = 1:2) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, rp = -1)
    return(x)
  },
  \(x, row = NULL, col = NULL, use = 1:2) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, tf = mean)
    return(x)
  }
)

myfun <- \(x, m) match(dimnames(x)[[m]][1], dimnames(x)[[m]])

for(f in funlist) {
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  
  # .M:
  expect_equal(
    f(x, ~ .M, ~ .M),
    f(x, 1, 2)
  ) |> errorfun()
  
  # .N:
  expect_equal(
    f(x, ~ .N, ~ .N),
    f(x, nr, nc)
  ) |> errorfun()
  
  # .Nms:
  expect_equal(
    f(x, ~ match(.Nms[1], .Nms), ~ match(.Nms[1], .Nms)),
    f(x, myfun(x, 1), myfun(x, 2))
  ) |> errorfun()
  
  # .bi():
  expect_equal(
    f(x, ~ .bi(1, -1), ~ .bi(1, -1)),
    f(x, c(1, nr), c(1, nc))
  ) |> errorfun()
  
  # .bi() + keyword
  expect_equal(
    f(x, ~ .bi(-.I), ~ .bi(-.I)),
    f(x, rev(seq_len(nr)), rev(seq_len(nc)))
  ) |> errorfun()
  
  enumerate <- enumerate + 5L
}


# data.table ====

x <- matrix(sample(1:100), 10, 10)
colnames(x) <- month.name[1:10]
x <- data.table::as.data.table(x)
expect_equal(
  ndim(x), 2L
)

funlist <- list(
  sbt_x,
  \(x, row = NULL, col = NULL, use = 1:2) {
    sbt_mod(x, row, col, use, chkdup = FALSE, rp = -1L)
  },
  \(x, row = NULL, col = NULL, use = 1:2) {
    sbt_mod(x, row, col, use, chkdup = FALSE, tf = mean)
  },
  \(x, row = NULL, col = NULL, use = 1:2) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, rp = -1L)
    return(x)
  },
  \(x, row = NULL, col = NULL, use = 1:2) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, tf = mean)
    return(x)
  }
)



for(f in funlist) {
  
  # these crash in all circumstances, except when rows are not specified
  nr <- nrow(x)
  nc <- ncol(x)
  
  
  # .M:
  expect_equal(
    f(x, ~ .M, ~ .M),
    f(x, 1, 2)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .M, ~ .M, -1),
    f(x, 1, 2, -1)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .M, ~ .M, -2),
    f(x, 1, 2, -2)
  ) |> errorfun()
  
  # .N:
  expect_equal(
    f(x, ~ .N, ~ .N),
    f(x, nr, nc)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .N, ~ .N, -1),
    f(x, nr, nc, -1)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .N, ~ .N, -2),
    f(x, nr, nc, -2)
  ) |> errorfun()
  
  # .Nms:
  expect_equal(
    f(x, 0L, ~ match(.Nms[1], .Nms)),
    f(x, 0L, myfun(x, 2))
  ) |> errorfun()
  expect_equal(
    f(x, 0L, ~ match(.Nms[1], .Nms), -2),
    f(x, 0L, myfun(x, 2), -2)
  ) |> errorfun()
  
  # .bi():
  expect_equal(
    f(x, ~ .bi(1, -1), ~ .bi(1, -1)),
    f(x, c(1, nr), c(1, nc))
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(1, -1), ~ .bi(1, -1), -1),
    f(x, c(1, nr), c(1, nc), -1)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(1, -1), ~ .bi(1, -1), -2),
    f(x, c(1, nr), c(1, nc), -2)
  ) |> errorfun()
  
  # .bi() + keyword
  expect_equal(
    f(x, ~ .bi(-.I), ~ .bi(-.I)),
    f(x, rev(seq_len(nr)), rev(seq_len(nc)))
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(-.I), ~ .bi(-.I), -1),
    f(x, rev(seq_len(nr)), rev(seq_len(nc)), -1)
  ) |> errorfun()
  expect_equal(
    f(x, ~ .bi(-.I), ~ .bi(-.I), -2),
    f(x, rev(seq_len(nr)), rev(seq_len(nc)), -2)
  ) |> errorfun()
  
  enumerate <- enumerate + 14L
}



