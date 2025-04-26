
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
    
    x <- mutatomic::mutatomic(x.data[[iD]])
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
      x <- mutatomic::mutatomic(x.data[[iD]])
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
    
    x <- mutatomic::mutatomic(x.data[[iD]])
    x2 <- x
    rp <- rev(x[ temp.ind ])
    sb_set(x, temp.ind, rp = rp)
    invisible(x) # waking up R
    expect_equal(
      x, x2
    ) |> errorfun()
    
    enumerate <- enumerate + 1
    
    if(!is.raw(x)) {
      x <- mutatomic::mutatomic(x.data[[iD]])
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

