
# set-up ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# vector ====
x <- as.mutatomic(integer(0L))
expect_equal(
  length(x), 0L
)

funlist <- list(
  \(x, ii, use = 1) ii_x(x, ii, use),
  \(x, ii, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = -1),
  \(x, ii, use = 1) ii_mod(x, ii, use, chkdup = FALSE, rp = integer(0L)),
  \(x, ii, use = 1) ii_mod(x, ii, use, chkdup = FALSE, tf = mean),
  \(x, ii, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = -1)
    return(x)
  },
  \(x, ii, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, rp = integer(0L))
    return(x)
  },
  \(x, ii, use = 1) {
    x <- data.table::copy(x)
    ii_set(x, ii, use, tf = mean)
    return(x)
  }
)

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)

for(f in funlist) {
  for(iUse in c(1, -1)) {
    for(iIndx in seq_along(indices)) {
      # main:
      expect_equal(
        f(x, indices[[iIndx]]),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx]], iUse),
        x
      ) |> errorfun()
      enumerate <- enumerate + 2L
      
    }
  }
}

# 1d array ====
x <- as.mutatomic(array(integer(0L)))
expect_equal(
  ndim(x), 1L
)

funlist <- list(
  \(x, ss, d = 1:ndim(x)) ss_x(x, ss, d),
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

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)

for(f in funlist) {
  for(iUse in c(1, -1)) {
    for(iIndx in seq_along(indices)) {
      # main:
      expect_equal(
        f(x, n(indices[[iIndx]])),
        x
      ) |> errorfun()
      expect_equal(
        f(x, n(indices[[iIndx]]), iUse),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx]]),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx]], iUse),
        x
      ) |> errorfun()
      enumerate <- enumerate + 4L
      
    }
  }
}


# 3d array ====

x <- as.mutatomic(array(integer(0L), c(0,0,0)))
expect_equal(
  ndim(x), 3L
)

funlist <- list(
  \(x, ss, d = 1:ndim(x)) ss_x(x, ss, d),
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

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)

for(f in funlist) {
  for(iIndx1 in seq_along(indices)) {
    for(iIndx2 in seq_along(indices)) {
      # main:
      expect_equal(
        f(x, n(indices[[iIndx1]], 0L, indices[[iIndx2]])),
        x
      ) |> errorfun()
      expect_equal(
        f(x, n(indices[[iIndx1]])),
        x
      ) |> errorfun()
      expect_equal(
        f(x, indices[[iIndx1]]),
        x
      ) |> errorfun()
      enumerate <- enumerate + 3L
    }
    
  }
}

for(f in funlist) {
  for(iUse in n(c(1, 3), c(-1, -3), c(1, -3), c(-1, 3))) {
    for(iIndx1 in seq_along(indices)) {
      for(iIndx2 in seq_along(indices)) {
        
        use <- iUse
        if(sample(c(TRUE, FALSE), 1)) {
          use <- rev(iUse)
        }
        
        expect_equal(
          f(x, n(indices[[iIndx1]], indices[[iIndx2]]), use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, n(indices[[iIndx1]]), use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, indices[[iIndx1]], use),
          x
        ) |> errorfun()
        
        enumerate <- enumerate + 3L
      }
    }
  }
}


for(f in funlist) {
  for(iUse in n(1:3, -1:-3, -3:-1, 3:1)) {
    for(iIndx1 in seq_along(indices)) {
      for(iIndx2 in seq_along(indices)) {
        
        expect_equal(
          f(x, n(indices[[iIndx1]], 0L, indices[[iIndx2]]), iUse),
          x
        ) |> errorfun()
        expect_equal(
          f(x, n(indices[[iIndx1]]), iUse),
          x
        ) |> errorfun()
        expect_equal(
          f(x, indices[[iIndx1]], iUse),
          x
        ) |> errorfun()
        
        enumerate <- enumerate + 3L

      }
    }
  }
}


# sbt, matrix ====

x <- as.mutatomic(matrix(integer(0L),0,0))
expect_equal(
  ndim(x), 2L
)

funlist <- list(
  \(x, row, col, d = 1:ndim(x)) sbt_x(x, row, col, d),
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

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)


for(f in funlist) {
  for(iUse in n(c(1, 2), c(-1, -2), c(1, -2), c(-1, 2))) {
    for(iIndx1 in seq_along(indices)) {
      for(iIndx2 in seq_along(indices)) {
        
        use <- iUse
        if(sample(c(TRUE, FALSE), 1)) {
          use <- rev(iUse)
        }
        
        expect_equal(
          f(x, indices[[iIndx1]], indices[[iIndx2]], use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, indices[[iIndx1]], 0L, use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, 0L, indices[[iIndx1]], use),
          x
        ) |> errorfun()
        
        enumerate <- enumerate + 3L
      }
    }
  }
}

# sbt, data.frame ====


x <- data.table::data.table(NULL)
expect_equal(
  ndim(x), 2L
)

funlist <- list(
  \(x, row, col, d = 1:ndim(x)) sbt_x(x, row, col, d),
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

indices <- n(logical(0L), integer(0L), character(0L), 0L, NULL)


for(f in funlist) {
  for(iUse in n(c(1, 2), c(-1, -2), c(1, -2), c(-1, 2))) {
    for(iIndx1 in seq_along(indices)) {
      for(iIndx2 in seq_along(indices)) {
        
        use <- iUse
        if(sample(c(TRUE, FALSE), 1)) {
          use <- rev(iUse)
        }
        
        expect_equal(
          f(x, indices[[iIndx1]], indices[[iIndx2]], use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, indices[[iIndx1]], 0L, use),
          x
        ) |> errorfun()
        expect_equal(
          f(x, 0L, indices[[iIndx1]], use),
          x
        ) |> errorfun()
        
        enumerate <- enumerate + 3L
      }
    }
  }
}


