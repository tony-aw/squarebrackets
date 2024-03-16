
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

# sb_set.matrix() ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  c(rnorm(96), NA, NaN, NA, NaN),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  rep(NA, 100),
  rep(c(NA, NaN), 50)
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
      x2[ temp.row, temp.col ] <- rev(x2[ temp.row, temp.col ])
      rp <- rev(x[ temp.row, temp.col ])
      sb_set(x, temp.row, temp.col, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      
      x <- mutable_atomic(x.data[[iD]], dim = c(10,10))
      x.len <- length(x[ temp.row, temp.col ])
      x2 <- x
      rp <- rep(NA, x.len)
      x2[ temp.row, temp.col ] <- rp
      sb_set(x, temp.row, temp.col, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      enumerate <- enumerate + 2
    }
  }
}


# setapply ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 90, TRUE),
  sample(c(1:88, NA, NA)),
  c(rnorm(86), NA, NaN, NA, NaN),
  sample(c(letters, LETTERS, NA, NA), 90, TRUE),
  rep(NA, 90),
  rep(c(NA, NaN), 45)
)

for(iD in 1:4) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:nrow(x)) x2[i,] <- rev(x2[i,]) 
  setapply(x, 1, rev)
  invisible(x) # waking up R
  expect_equal(
    x, as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}

for(iD in 1:4) {
  x <- mutable_atomic(x.data[[iD]], dim = c(9,10))
  x2 <- x
  for(i in 1:ncol(x)) x2[,i] <- rev(x2[,i]) 
  setapply(x, 2, rev)
  expect_equal(
    x, as.mutable_atomic(x2)
  ) |> errorfun()
  enumerate <- enumerate + 1
}

