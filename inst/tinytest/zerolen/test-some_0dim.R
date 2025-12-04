
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# 3d array ====

x <- as.mutatomic(array(integer(0L), c(0,10,0)))
dimnames(x) <- list(
  character(0L),
  month.abb[1:10],
  character(0L)
)
expect_equal(
  ndim(x), 3L
)


expect_equal(
  ss_x(x, 1:5, 2),
  x[, 1:5, , drop = FALSE]
)
expect_equal(
  ss_x(x, 1:5, -2),
  x[, -1:-5, , drop = FALSE]
)
enumerate <- enumerate + 2L


funlist <- list(
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, rp = -1),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, rp = integer(0L)),
  \(x, ss, d = 1:ndim(x)) ss_mod(x, ss, d, chkdup = FALSE, tf = mean),
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, rp = -1)
    return(x)
  },
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, rp = integer(0L))
    return(x)
  },
  \(x, ss, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    ss_set(x, ss, d, tf = mean)
    return(x)
  }
)

for(f in funlist) {
  expect_equal(
    f(x, 1:5, 2L),
    x
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, -2L),
    x
  ) |> errorfun()
  enumerate <- enumerate + 2L
}




# sbt, matrix ====

x <- as.mutatomic(matrix(integer(0L),0,10))
expect_equal(
  ndim(x), 2L
)

expect_equal(
  sbt_x(x, col = 1:5),
  x[, 1:5, drop = FALSE]
)
expect_equal(
  sbt_x(x, 0L, 1:5, -2),
  x[, -1:-5, drop = FALSE]
)


funlist <- list(
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, rp = -1),
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, rp = integer(0L)),
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, tf = mean),
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, rp = -1)
    return(x)
  },
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, rp = integer(0L))
    return(x)
  },
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, tf = mean)
    return(x)
  }
)

for(f in funlist) {
  expect_equal(
    f(x, 0L, 1:5, 2L),
    x
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:5, -2L),
    x
  ) |> errorfun()
  enumerate <- enumerate + 2L
}



# sbt, data.frame ====


x <- matrix(integer(0L),0,10) |> data.table::as.data.table()
expect_equal(
  ndim(x), 2L
)

expect_equal(
  sbt_x(x, col = 1:5),
  x[, 1:5, drop = FALSE]
)
expect_equal(
  sbt_x(x, 0L, 1:5, -2),
  x[, -1:-5, drop = FALSE]
)


funlist <- list(
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, rp = -1),
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, rp = integer(0L)),
  \(x, row, col, d = 1:ndim(x)) sbt_mod(x, row, col, d, chkdup = FALSE, tf = mean),
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, rp = -1)
    return(x)
  },
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, rp = integer(0L))
    return(x)
  },
  \(x, row, col, d = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, d, tf = mean)
    return(x)
  }
)

for(f in funlist) {
  expect_equal(
    f(x, 0L, 1:5, 2L),
    x
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:5, -2L),
    x
  ) |> errorfun()
  enumerate <- enumerate + 2L
}




