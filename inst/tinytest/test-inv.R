
# Note: inversion tests on regular vectors is already done in the generics and generics2 test folders
# this test script is for multi-dimensional objects only,
# since there inversion can happen individually per dimension

# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
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
  
  
  # numeric:
  expect_equal(
    f(x, 1:10, -1),
    f(x, 11:100, 1)
  ) |> errorfun()
  
  
  # character:
  expect_equal(
    f(x, letters, -1),
    f(x, !names(x) %in% letters, 1)
  ) |> errorfun()
  
  
  # logical:
  s <- sample(c(TRUE, FALSE), 100, TRUE)
  expect_equal(
    f(x, s, -1),
    f(x, !s, 1)
  ) |> errorfun()
  
  
  # empty selection:
  expect_equal(
    f(x, integer(0L), -1),
    f(x, 1:length(x))
  ) |> errorfun()
  
  
  # missing args:
  expect_equal(
    f(x, use = Inf),
    f(x, use = -Inf)
  ) |> errorfun()
  
  enumerate <- enumerate + 5L
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
  
  
  # numeric:
  expect_equal(
    f(x, n(1:4, 1:2), c(-1, -3)),
    f(x, n(5:6, 3:4), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(1:4, 1:2), c(1, -3)),
    f(x, n(1:4, 3:4), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(1:4, 3:4), c(-1, 3)),
    f(x, n(5:6, 3:4), c(1, 3))
  ) |> errorfun()
  
  
  # character:
  expect_equal(
    f(x, n(letters[1:4], month.abb[1:2]), c(-1, -3)),
    f(x, n(letters[5:6], month.abb[3:4]), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(letters[5:6], month.abb[1:2]), c(1, -3)),
    f(x, n(letters[5:6], month.abb[3:4]), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(letters[1:4], month.abb[3:4]), c(-1, 3)),
    f(x, n(letters[5:6], month.abb[3:4]), c(1, 3))
  ) |> errorfun()
  
  
  # logical:
  s <- lapply(dim(x), \(i)sample(c(TRUE, FALSE), i, TRUE))[c(1, 3)]
  s1 <- s[[1]]
  s2 <- s[[2]]
  expect_equal(
    f(x, n(!s1, !s2), c(-1, -3)),
    f(x, n(s1, s2), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(s1, !s2), c(1, -3)),
    f(x, n(s1, s2), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(!s1, s2), c(-1, 3)),
    f(x, n(s1, s2), c(1, 3))
  ) |> errorfun()
  
  
  # empty selection:
  expect_equal(
    f(x, n(integer(0L), integer(0L)), c(-1, -3)),
    f(x, n(~1:.N, ~ 1:.N), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(~ 1:.N, integer(0L)), c(1, -3)),
    f(x, n(~1:.N, ~ 1:.N), c(1, 3))
  ) |> errorfun()
  expect_equal(
    f(x, n(integer(0L), ~ 1:.N), c(-1, 3)),
    f(x, n(~1:.N, ~ 1:.N), c(1, 3))
  ) |> errorfun()
  
  
  # missing args:
  expect_equal(
    f(x, use = Inf),
    f(x, use = -Inf)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L * 3L + 1L
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



for(f in funlist) {
  
  
  # numeric:
  expect_equal(
    f(x, 1:4, 1:5, -1:-2),
    f(x, 5:10, 6:10, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:4, 1:5, -2),
    f(x, 1:4, 6:10, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:4, 1:5, -1),
    f(x, 5:10, 1:5, 1:2)
  ) |> errorfun()
  
  
  # character:
  expect_equal(
    f(x, month.abb[1:4], month.name[1:3], -1:-2),
    f(x, month.abb[5:10], month.name[4:10], 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, month.abb[5:10], month.name[1:3], -2),
    f(x, month.abb[5:10], month.name[4:10], 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, month.abb[1:4], month.name[4:10], -1),
    f(x, month.abb[5:10], month.name[4:10], 1:2)
  ) |> errorfun()
  
  
  # logical:
  s <- lapply(dim(x), \(i)sample(c(TRUE, FALSE), i, TRUE))[1:2]
  s1 <- s[[1]]
  s2 <- s[[2]]
  expect_equal(
    f(x, !s1, !s2, -1:-2),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, s1, !s2, -2),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, !s1, s2, -1),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  
  
  # empty selection:
  expect_equal(
    f(x, integer(0L), integer(0L), -1:-2),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, ~ 1:.N, integer(0L), -2),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, integer(0L), ~ 1:.N, -1),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  
  
  # missing args:
  expect_equal(
    f(x, use = 1:2),
    f(x, use = -1:-2)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L * 3L + 1L
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
  
  # numeric:
  expect_equal(
    f(x, 1:4, 1:5, -1:-2),
    f(x, 5:10, 6:10, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:4, 1:5, -2), 
    f(x, 1:4, 6:10, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:4, 1:5, -1),
    f(x, 5:10, 1:5, 1:2)
  ) |> errorfun()
  
  
  # character:
  expect_equal(
    f(x, 0L, month.name[1:3], -2),
    f(x, 0L, month.name[4:10], 1:2)
  ) |> errorfun()
  
  
  # logical:
  s <- lapply(dim(x), \(i)sample(c(TRUE, FALSE), i, TRUE))[1:2]
  s1 <- s[[1]]
  s2 <- s[[2]]
  expect_equal(
    f(x, !s1, !s2, -1:-2),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, s1, !s2, -2),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, !s1, s2, -1),
    f(x, s1, s2, 1:2)
  ) |> errorfun()
  
  
  # empty selection:
  expect_equal(
    f(x, integer(0L), integer(0L), -1:-2),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, ~ 1:.N, integer(0L), -2),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  expect_equal(
    f(x, integer(0L), ~ 1:.N, -1),
    f(x, ~1:.N, ~ 1:.N, 1:2)
  ) |> errorfun()
  
  
  # missing args:
  expect_equal(
    f(x, use = 1:2),
    f(x, use = -1:-2)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L * 3L + 1L
}



