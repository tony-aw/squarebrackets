
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}



# MULTI-DIM TETSS ====

funlist <- list(
  \(x, row, col, use = 1:ndim(x)) sbt_x(x, row, col, use),
  \(x, row, col, use = 1:ndim(x)) sbt_mod(x, row, col, use, chkdup = FALSE, rp = -1),
  \(x, row, col, use = 1:ndim(x)) sbt_mod(x, row, col, use, chkdup = FALSE, tf = \(x)x[1]),
  \(x, row, col, use = 1:ndim(x)) {
    x <- as.mutatomic(x)
    sbt_set(x, row, col, use, rp = -1)
    return(x)
  },
  \(x, row, col, use = 1:ndim(x)) {
    x <- as.mutatomic(x)
    sbt_set(x, row, col, use, tf = \(x) x[1])
    return(x)
  }
)

x <- matrix(1:20, 5, 4)

for(f in funlist) {
  
  
  # use equivalences ====
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 2:1)
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 1)
  ) |> errorfun()
  
  expect_equal( # exclude all
    f(x, 1:5, 1:4, -1:-2),
    f(x, 1:5, 1:4, -2:-1)
  ) |> errorfun()
  
  expect_equal(
    f(x, 1:5, 1:4, -2),
    f(x, 1:5, 1:4, c(1, -2))
  ) |> errorfun()
  expect_equal(
      f(x, 1:5, 1:4, -2),
      f(x, 1:5, 1:4, c(-2, 1))
  ) |> errorfun()
  
  expect_equal(
    f(x, 1:5, 1:4, -1),
    f(x, 1:5, 1:4, c(-1, 2))
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, -1),
    f(x, 1:5, 1:4, c(2, -1))
  ) |> errorfun()
  
  expect_error(
    f(x, 1:3, 1:4, 1:4),
    pattern = "improper `use` specified",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 9L
  
  
  
  # check NULL/0L equivalence ====
  expect_equal(
    f(x, 0L, 1:3),
    f(x, NULL, 1:3)
  ) |> errorfun()
  expect_equal(
    f(x, 1:3, 0L),
    f(x, 1:3, NULL)
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:3, -1:-2),
    f(x, NULL, 1:3, -1:-2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:3, 0L, -1:-2),
    f(x, 1:3, NULL, -1:-2)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
  # general error tests ====
  expect_error(
    f(x, 1:3, 1:3, "a")
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
}


# DATA.FRAME TESTS ====


funlist <- list(
  \(x, row, col, use = 1:ndim(x)) sbt_x(x, row, col, use),
  \(x, row, col, use = 1:ndim(x)) sbt_mod(x, row, col, use, chkdup = FALSE, rp = -1),
  \(x, row, col, use = 1:ndim(x)) sbt_mod(x, row, col, use, chkdup = FALSE, tf = \(x)x[1]),
  \(x, row, col, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, rp = -1)
    return(x)
  },
  \(x, row, col, use = 1:ndim(x)) {
    x <- data.table::copy(x)
    sbt_set(x, row, col, use, tf = \(x) x[1])
    return(x)
  }
)

x <- matrix(1:20, 5, 4) |> data.table::as.data.table()

for(f in funlist) {
  
  
  # use equivalences ====
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 2:1)
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, 1:2),
    f(x, 1:5, 1:4, 1)
  ) |> errorfun()
  
  expect_equal( # exclude all
    f(x, 1:5, 1:4, -1:-2),
    f(x, 1:5, 1:4, -2:-1)
  ) |> errorfun()
  
  expect_equal(
    f(x, 1:5, 1:4, -2),
    f(x, 1:5, 1:4, c(1, -2))
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, -2),
    f(x, 1:5, 1:4, c(-2, 1))
  ) |> errorfun()
  
  expect_equal(
    f(x, 1:5, 1:4, -1),
    f(x, 1:5, 1:4, c(-1, 2))
  ) |> errorfun()
  expect_equal(
    f(x, 1:5, 1:4, -1),
    f(x, 1:5, 1:4, c(2, -1))
  ) |> errorfun()
  
  expect_error(
    f(x, 1:3, 1:4, 1:4),
    pattern = "improper `use` specified",
    fixed = TRUE
  ) |> errorfun()
  
  enumerate <- enumerate + 9L
  
  
  
  # check NULL/0L equivalence ====
  expect_equal(
    f(x, 0L, 1:3),
    f(x, NULL, 1:3)
  ) |> errorfun()
  expect_equal(
    f(x, 1:3, 0L),
    f(x, 1:3, NULL)
  ) |> errorfun()
  expect_equal(
    f(x, 0L, 1:3, -1:-2),
    f(x, NULL, 1:3, -1:-2)
  ) |> errorfun()
  expect_equal(
    f(x, 1:3, 0L, -1:-2),
    f(x, 1:3, NULL, -1:-2)
  ) |> errorfun()
  
  enumerate <- enumerate + 4L
  
  # general error tests ====
  expect_error(
    f(x, 1:3, 1:3, "a")
  ) |> errorfun()
  enumerate <- enumerate + 1L
  
  
}




