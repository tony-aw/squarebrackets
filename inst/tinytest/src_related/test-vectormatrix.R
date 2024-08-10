
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

# vector ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
indices <- list(
  2, 10:5, 1:100, NULL
)

# test functionality

for(iD in 1:length(x.data)) {
  for(iIndices in 1:length(indices)) {
    temp.ind <- indices[[iIndices]]
    if(is.null(temp.ind)) temp.ind <- 1:100
    
    x <- mutable_atomic(x.data[[iD]])
    x2 <- x
    x2[ temp.ind ] <- rev(x2[ temp.ind ])
    rp <- rev(x[ temp.ind ])
    sb_set(x, temp.ind, rp = rp)
    invisible(x) # waking up R
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 1
    
    if(!is.raw(x)) {
      x <- mutable_atomic(x.data[[iD]])
      x.len <- length(x[ temp.ind ])
      x2 <- x
      rp <- rep(NA, x.len)
      x2[ temp.ind ] <- rp
      sb_set(x, temp.ind, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      enumerate <- enumerate + 1
    }
    
  }
}

# test if object is changed by reference

for(iD in 1:length(x.data)) {
  for(iIndices in 1:length(indices)) {
    temp.ind <- indices[[iIndices]]
    if(is.null(temp.ind)) temp.ind <- 1:100
    
    x <- mutable_atomic(x.data[[iD]])
    x2 <- x
    rp <- rev(x[ temp.ind ])
    sb_set(x, temp.ind, rp = rp)
    invisible(x) # waking up R
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 1
    
    if(!is.raw(x)) {
      x <- mutable_atomic(x.data[[iD]])
      x.len <- length(x[ temp.ind ])
      x2 <- x
      rp <- rep(NA, x.len)
      sb_set(x, temp.ind, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      enumerate <- enumerate + 1
    }
    
  }
}


# matrix ====
x.data <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:98, NA, NA)),
  rnorm(100),
  sample(c(NA, NaN, -Inf, Inf, 0), 100, TRUE),
  sample(c(rnorm(50), NA, NaN, Inf, -Inf), 100, TRUE),
  sample(c(letters, LETTERS, NA, NA), 100, TRUE),
  as.complex(c(1:99, NA)),
  as.raw(0:99),
  rep(NA, 100)
)
x.nrow <- 10
x.ncol <- 10
rows <- list(
  2, 10:5, 1:x.nrow, NULL
)
cols <- list(
  2, 10:5, 1:x.ncol, NULL
)

# test functionality

for(iD in 1:length(x.data)) {
  for(iRow in 1:length(rows)) {
    for(iCol in 1:length(cols)) {
      temp.row <- rows[[iRow]]
      temp.col <- cols[[iCol]]
      if(is.null(temp.row)) temp.row <- 1:x.nrow
      if(is.null(temp.col)) temp.col <- 1:x.ncol
      
      x <- mutable_atomic(x.data[[iD]], dim = c(10,10))
      x2 <- x
      x2[ temp.row, temp.col ] <- rev(x2[ temp.row, temp.col ])
      rp <- rev(x[ temp.row, temp.col ])
      sb_set(x, temp.row, temp.col, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      enumerate <- enumerate + 1
      
      if(!is.raw(x)) {
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
        
        enumerate <- enumerate + 1
      }
      
    }
  }
}


# test if object is changed by reference

for(iD in 1:length(x.data)) {
  for(iRow in 1:length(rows)) {
    for(iCol in 1:length(cols)) {
      temp.row <- rows[[iRow]]
      temp.col <- cols[[iCol]]
      if(is.null(temp.row)) temp.row <- 1:x.nrow
      if(is.null(temp.col)) temp.col <- 1:x.ncol
      
      x <- mutable_atomic(x.data[[iD]], dim = c(10,10))
      x2 <- x
      rp <- rev(x[ temp.row, temp.col ])
      sb_set(x, temp.row, temp.col, rp = rp)
      invisible(x) # waking up R
      expect_equal(
        x, x2
      ) |> errorfun()
      
      enumerate <- enumerate + 1
      
      if(!is.raw(x)) {
        x <- mutable_atomic(x.data[[iD]], dim = c(10,10))
        x.len <- length(x[ temp.row, temp.col ])
        x2 <- x
        rp <- rep(NA, x.len)
        sb_set(x, temp.row, temp.col, rp = rp)
        invisible(x) # waking up R
        expect_equal(
          x, x2
        ) |> errorfun()
        
        enumerate <- enumerate + 1
      }
      
    }
  }
}



# array with d = 4 dimensions ====

n <- 10^4
x.data <- list(
  sample(c(TRUE, FALSE, NA), n, TRUE),
  sample(c(seq_len(n-2), NA, NA)),
  rnorm(n),
  sample(c(NA, NaN, -Inf, Inf, 0), n, TRUE),
  sample(c(letters, LETTERS, NA, NA), n, TRUE),
  as.complex(c(seq_len(n-1), NA)),
  sample(as.raw(0:10), n, TRUE),
  rep(NA, n)
)

subset_arr <- function(x, i, j, l, rp) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  x[i, j, , l] <- rp
  return(x)
}

make_rp <- function(len) {
  return(rev(x[1:len]))
}


# test functionality

for(iD in 1:length(x.data)) {
  
  # set 1
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  
  # set 2
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  # set 3
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), c(1:4), rep(FALSE, 10))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  enumerate <- enumerate + 3
  
  
  if(!is.raw(x)) {
    # set 1
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    
    # set 2
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    
    # set 3
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), c(1:4), rep(FALSE, 10))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    x2 <- subset_arr(x2, sub[[1]], sub[[2]], sub[[3]], rp)
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 3
    
  }
  
}


# test if object is modified by reference


for(iD in 1:length(x.data)) {
  
  # set 1
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  
  # set 2
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  # set 3
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  sub <- list(c("a"), c(1:4), rep(FALSE, 10))
  dims <- c(1,2,4)
  len <- length(sb_x(x, sub, dims))
  
  x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
  rownames(x) <- c(letters[1:8], "a", NA)
  rp <- make_rp(len)
  x2 <- x
  sb_set(x, sub, dims, rp = rp)
  expect_equal(
    x, x2
  ) |> errorfun()
  
  enumerate <- enumerate + 3
  
  
  if(!is.raw(x)) {
    # set 1
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    
    # set 2
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    
    # set 3
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    sub <- list(c("a"), c(1:4), rep(FALSE, 10))
    dims <- c(1,2,4)
    len <- length(sb_x(x, sub, dims))
    
    x <- mutable_atomic(x.data[[iD]], dim = c(10, 10, 10, 10))
    rownames(x) <- c(letters[1:8], "a", NA)
    rp <- NA
    x2 <- x
    sb_set(x, sub, dims, rp = rp)
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 3
    
  }
  
}


