
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# vector ====
x <- as.mutatomic(1:100)
names(x) <- sample(c(month.name, month.abb), 100, TRUE)

funlist <- list(
  \(x, ii = NULL, use = 1) ii_x(x, ii, use),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = -1),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = -1:-100),
  \(x, ii = NULL, use = 1) ii_mod(x, ii, use, chkdup = FALSE, tf = mean),
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = -1)
    return(x)
  },
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = -1:-100)
    return(x)
  },
  \(x, ii = NULL, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, tf = mean)
    return(x)
  }
)

for(f in funlist) {
  # main:
  expect_equal(
    f(x),
    f(x, 1:100)
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
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
  # main:
  expect_equal(
    f(x),
    f(x, 1:100)
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
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
  
  expect_equal(
    f(x),
    f(x, ~ .I)
  ) |> errorfun()
  expect_equal(
    f(x, n(1:2, 0L, 1:2)),
    f(x, n(1:2, ~ .I, 1:2))
  ) |> errorfun()
  expect_equal(
    f(x, 1:2, c(1, 3)),
    f(x, n(1:2, ~ .I, 1:2))
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
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
  
  expect_equal(
    f(x, 0L, 0L),
    f(x, ~ .I, ~ .I)
  ) |> errorfun()
  expect_equal(
    f(x, 1:2, 0L),
    f(x, 1:2, ~ .I)
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:2),
    f(x, ~ .I, 1:2)
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
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
  
  expect_equal(
    f(x, 0L, 0L),
    f(x, ~ .I, ~ .I)
  ) |> errorfun()
  expect_equal(
    f(x, 1:2, 0L),
    f(x, 1:2, ~ .I)
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:2),
    f(x, ~ .I, 1:2)
  ) |> errorfun()
  
  enumerate <- enumerate + 3L
}


