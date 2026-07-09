
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  
  if(isFALSE(tt)) stop(print(tt))
}


# errors ====
expect_error(
  arepl(1:10, list(1:10), -10)
)

x <- array(1:10)
expect_error(
  arepl(x, 1:10, -10),
  pattern = "`sub` must be a list"
)

enumerate <- enumerate + 1L


# 1d array ====
for(iN in 1:10) {
  x <- array(1:10)
  x2 <- data.table::copy(x)
  
  arepl(x, list(1:iN), -1)
  x2[1:iN] <- -1
  expect_equal(
    x, x2
  ) |> errorfun()
}


# matrix ====
for(iR in 1:5) {
  for(iC in 1:4) {
    x <- matrix(1:20, 5, 4)
    x2 <- data.table::copy(x)
    
    arepl(x, list(1:iR, 1:iC), -1)
    x2[1:iR, 1:iC] <- -1
    expect_equal(
      x, x2
    ) |> errorfun()
  }
}
