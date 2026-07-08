
# setup ====

enumerate <- 0L

Lens <- c(1, 10, 32, 100, 2^16, 2^16+1)

x.gendata <- list(
  function(n) sample(c(TRUE, FALSE, NA), n, TRUE),
  function(n) sample(c(1:10, NA_integer_), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE) + -1i * sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE),
  function(n) sample(c(letters, NA), n, TRUE),
  function(n) sample(as.raw(0:255), n, TRUE)
)
tf.list <- list(
  \(x) !x,
  \(x) -x,
  \(x) -x,
  \(x) -x,
  toupper,
  \(x) !x
)
rp.gendata <- list(
  function(n) sample(c(TRUE, FALSE, NA), n, TRUE),
  function(n) sample(c(1:10, NA_integer_), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE) + -1i * sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE),
  function(n) sample(c(letters, NA), n, TRUE),
  function(n) sample(as.raw(0:255), n, TRUE)
)


# single ====

y.gendata <- list(
  function(n) sample(c(TRUE, FALSE, NA), n, TRUE),
  function(n) sample(c(1:10, NA_integer_), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE),
  function(n) sample(c(letters, NA), n, TRUE)
)
v.list <- list(
  FALSE,
  2L,
  2.5,
  "a"
)

expected <- out <- vector("list", length(Lens) * length(x.gendata) * length(y.gendata) * 3 * 2)
counter <- 1L

for(iLen in Lens) {
  for(iX in seq_along(x.gendata)) {
    for(iY in seq_along(y.gendata)) {
      for(iNA in c(TRUE, FALSE, NA)) {
        for(iUse in c(1, -1)) {
          
          x <- as.mutatomic(x.gendata[[iX]](iLen))
          x2 <- data.table::copy(x)
          
          y <- y.gendata[[iY]](iLen)
          v <- v.list[[iY]]
          len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
          rp <- rp.gendata[[iX]](len)
          tf <- tf.list[[iX]]
          
          # expect_equal(
          #   basetest_single(x2, y = y, v = v, na = iNA, use = iUse),
          #   slicetest(x, y = y, v = v, na = iNA, use = iUse)
          # ) |> errorfun()
          
          expected[[counter]] <- basetest_single(x2, y = y, v = v, na = iNA, use = iUse)
          out[[counter]] <- slicetest(x, y = y, v = v, na = iNA, use = iUse)
          
          counter <- counter + 1L
          
        }
      }
    }
  }
}

print("slicev single")

expect_equal(
  expected, out
)
enumerate <- enumerate + counter


# numeric range ===
y.gendata <- list(
  function(n) sample(c(1:10, NA_integer_), n, TRUE),
  function(n) sample(c(1.5:10.5, Inf, -Inf, NA, NaN), n, TRUE)
)
min.list <- list(
  -Inf,
  2.1,
  -Inf,
  2.1
)
max.gendata <- list(
  Inf,
  10,
  10,
  Inf
)

expected <- out <- vector("list", length(Lens) * length(x.gendata) * length(y.gendata) * 3 * 2)
counter <- 1L

for(iLen in Lens) {
  for(iX in seq_along(x.gendata)) {
    for(iY in seq_along(y.gendata)) {
      for(j in seq_along(min.list)) {
        for(iNA in c(TRUE, FALSE, NA)) {
          for(iUse in c(1, -1)) {
            
            x <- as.mutatomic(x.gendata[[iX]](iLen))
            x2 <- data.table::copy(x)
            
            y <- y.gendata[[iY]](iLen)
            v <- c(min.list[[j]], max.gendata[[j]])
            
            tf <- tf.list[[iX]]
            len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
            rp <- rp.gendata[[iX]](len)
            
            # expect_equal(
            #   basetest_numrng(x2, y = y, v = v, na = iNA, use = iUse),
            #   slicetest(x, y = y, v = v, na = iNA, use = iUse)
            # ) |> errorfun()
            
            expected[[counter]] <- basetest_numrng(x2, y = y, v = v, na = iNA, use = iUse)
            out[[counter]] <- slicetest(x, y = y, v = v, na = iNA, use = iUse)
            counter <- counter + 1L
            
          }
        }
      }
    }
  }
}

print("slicev numeric range")

expect_equal(
  expected, out
)
enumerate <- enumerate + counter



# string, multiple ====

expected <- out <- vector("list", length(Lens) * length(x.gendata) * length(y.gendata) * 3 * 2)
counter <- 1L

for(iLen in Lens) {
  for(iX in seq_along(x.gendata)) {
    for(iNA in c(TRUE, FALSE, NA)) {
      for(iUse in c(1, -1)) {
        
        
        x <- as.mutatomic(x.gendata[[iX]](iLen))
        x2 <- data.table::copy(x)
        
        y <- sample(c(month.abb, NA), iLen, TRUE)
        v <- sample(month.abb, 6L)
        
        tf <- tf.list[[iX]]
        len <- eval_stride(stride_v(y, v = v, na = iNA, use = iUse), x)$len
        rp <- rp.gendata[[iX]](len)
        
        # expect_equal(
        #   basetest_str(x2, y = y, v = v, na = iNA, use = iUse),
        #   slicetest(x, y = y, v = v, na = iNA, use = iUse)
        # ) |> errorfun()
        
        expected[[counter]] <- basetest_str(x2, y = y, v = v, na = iNA, use = iUse)
        out[[counter]] <- slicetest(x, y = y, v = v, na = iNA, use = iUse)
        
        counter <- counter + 1L
        
      }
    }
    
  }
}

print("slicev multiple strings")

expect_equal(
  expected, out
)
enumerate <- enumerate + counter

