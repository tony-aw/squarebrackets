
# setup ====

enumerate <- 0L

x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE) + -1i * sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  sample(as.raw(0:255), 100, TRUE)
)
tf.list <- list(
  \(x) !x,
  \(x) -x,
  \(x) -x,
  \(x) -x,
  toupper,
  \(x) !x
)
rp.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE) + -1i * sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE),
  sample(as.raw(0:255), 100, TRUE)
)


# single ====

y.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE)
)
v.list <- list(
  FALSE,
  2L,
  2.5,
  "a"
)


for(iX in seq_along(x.list)) {
  for(iY in seq_along(y.list)) {
    for(iNA in c(TRUE, FALSE, NA)) {
      for(iUse in c(1, -1)) {
        
        x <- as.mutatomic(x.list[[iX]])
        x2 <- data.table::copy(x)
        
        y <- y.list[[iY]]
        v <- v.list[[iY]]
        len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
        rp <- rp.list[[iX]][1:sample(c(1L, len), 1L)]
        tf <- tf.list[[iX]]
        
        expect_equal(
          basetest_single(x2, y = y, v = v, na = iNA, use = iUse),
          slicetest(x, y = y, v = v, na = iNA, use = iUse)
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
    }
  }
}


# numeric range ===
y.list <- list(
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE)
)
min.list <- list(
  -Inf,
  2.1,
  -Inf,
  2.1
)
max.list <- list(
  Inf,
  10,
  10,
  Inf
)
for(iX in seq_along(x.list)) {
  for(iY in seq_along(y.list)) {
    for(j in seq_along(min.list)) {
      for(iNA in c(TRUE, FALSE, NA)) {
        for(iUse in c(1, -1)) {
          
          x <- as.mutatomic(x.list[[iX]])
          x2 <- data.table::copy(x)
          
          y <- y.list[[iY]]
          v <- c(min.list[[j]], max.list[[j]])
          
          tf <- tf.list[[iX]]
          len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
          rp <- rp.list[[iX]][1:sample(c(1L, len), 1L)]
          
          expect_equal(
            basetest_numrng(x2, y = y, v = v, na = iNA, use = iUse),
            slicetest(x, y = y, v = v, na = iNA, use = iUse)
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
          
        }
      }
    }
  }
}


# string, multiple ====
for(iX in seq_along(x.list)) {
  for(iNA in c(TRUE, FALSE, NA)) {
    for(iUse in c(1, -1)) {
      
      
      x <- as.mutatomic(x.list[[iX]])
      x2 <- data.table::copy(x)
      
      y <- sample(c(month.abb, NA), 100, TRUE)
      v <- sample(month.abb, 6L)
      
      tf <- tf.list[[iX]]
      len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
      rp <- rp.list[[iX]][1:sample(c(1L, len), 1L)]
      
      expect_equal(
        basetest_str(x2, y = y, v = v, na = iNA, use = iUse),
        slicetest(x, y = y, v = v, na = iNA, use = iUse)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
  
}


