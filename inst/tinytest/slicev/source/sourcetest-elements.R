
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


# single ====

x.list <- list(
  sample(c(TRUE, FALSE, NA), 100, TRUE),
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE),
  sample(c(letters, NA), 100, TRUE)
)
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
tf.list <- list(
  \(x) !x,
  \(x) -x,
  \(x) -x,
  toupper
)

for(i in seq_along(x.list)) {
  for(iNA in c(TRUE, FALSE, NA)) {
    for(iR in c(TRUE, FALSE)) {
      
      x <- as.mutatomic(x.list[[i]])
      x2 <- data.table::copy(x)
      
      y <- y.list[[i]]
      v <- v.list[[i]]
      rp <- y.list[[i]][1]
      tf <- tf.list[[i]]
      
      expect_equal(
        basetest_single(x2, y = y, v = v, na = iNA, r = iR),
        slicetest(x, y = y, v = v, na = iNA, r = iR)
      ) |> errorfun()
      
      enumerate <- enumerate + 1L
      
    }
  }
  
  
}


# numeric range ===
x.list <- list(
  sample(c(1:10, NA_integer_), 100, TRUE),
  sample(c(1.5:10.5, Inf, -Inf, NA, NaN), 100, TRUE)
)
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
for(i in seq_along(x.list)) {
  for(j in seq_along(min.list)) {
    for(iNA in c(TRUE, FALSE, NA)) {
      for(iR in c(TRUE, FALSE)) {
        
        x <- as.mutatomic(x.list[[i]])
        x2 <- data.table::copy(x)
        
        y <- y.list[[i]]
        v <- c(min.list[[j]], max.list[[j]])
        
        tf <- \(x) -x
        rp <- y.list[[i]][1]
        
        expect_equal(
          basetest_numrng(x2, y = y, v = v, na = iNA, r = iR),
          slicetest(x, y = y, v = v, na = iNA, r = iR)
        ) |> errorfun()
        
        enumerate <- enumerate + 1L
        
      }
    }
  }
  
}


# string, multiple ====
tf <- toupper
rp <- "XXX"
for(iNA in c(TRUE, FALSE, NA)) {
  for(iR in c(TRUE, FALSE)) {
    
    
    x <- sample(c(letters, NA), 100, TRUE) |> as.mutatomic()
    x2 <- data.table::copy(x)
    
    y <- sample(c(month.abb, NA), 100, TRUE)
    v <- sample(month.abb, 6L)
    
    expect_equal(
      basetest_str(x2, y = y, v = v, na = iNA, r = iR),
      slicetest(x, y = y, v = v, na = iNA, r = iR)
    ) |> errorfun()
    
    enumerate <- enumerate + 1L
    
  }
}


