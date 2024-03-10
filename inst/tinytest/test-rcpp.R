
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

# sb_set.matrix() ====
x.data <- list(
  sample(c(TRUE, FALSE), 100, TRUE),
  sample(1:100),
  rnorm(100),
  sample(c(letters, LETTERS), 100, TRUE)
)
x.nrow <- 10
x.ncol <- 10
rows <- list(
  2, 5:10, 1:x.nrow, NULL
)
cols <- list(
  2, 5:10, 1:x.ncol, NULL
)

for(iD in 1:4) {
  for(iRow in 1:3) {
    for(iCol in 1:3) {
      temp.row <- rows[[iRow]]
      temp.col <- cols[[iCol]]
      if(is.null(temp.row)) temp.row <- 1:nrow(x)
      if(is.null(temp.col)) temp.col <- 1:ncol(x)
      
      x <- mutable_atomic(x.data[[iD]], dim = c(10,10))
      x2 <- x
      x2[ temp.row, temp.col ] <- sort(x2[ temp.row, temp.col ])
      rp <- sort(x[ temp.row, temp.col ])
      sb_set(x, row = rows[[iRow]], col = cols[[iCol]], rp = rp)
      expect_equal(
        x,
        x2
      ) |> errorfun()
      enumerate <- enumerate + 1
    }
  }
}


# setapply ====
x.data <- list(
  sample(c(TRUE, FALSE), 90, TRUE),
  sample(1:90),
  rnorm(90),
  sample(c(letters, LETTERS), 90, TRUE)
)

for(iD in 1:4) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:nrow(x)) x2[i,] <- sort(x2[i,]) 
  setapply(x, 1, sort)
  expect_equal(
    x,
    as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}

for(iD in 1:4) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:ncol(x)) x2[,i] <- sort(x2[,i]) 
  setapply(x, 2, sort)
  expect_equal(
    x,
    as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}
