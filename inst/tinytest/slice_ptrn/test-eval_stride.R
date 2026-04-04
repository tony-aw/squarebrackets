
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# vector checks ====
x <- 1:10
for(iFrom in 1:10) {
  for(iTo in 1:10) {
    for(iBy in 2:10) {
      for(iUse in c(-1, 1)) {
        by <- iBy
        from <- iFrom
        to <- iTo
        pattern = rep(c(TRUE, FALSE), ceiling(by/2)) |> sample(by)
        if(length(pattern) <= length(from:to)) {
          
          from <- iFrom
          to <- iTo
          
          stride <- stride_ptrn(from, to, pattern)
          ind <- (from:to)[pattern]
          es <- eval_stride(stride, x, iUse)
          
          expect_equal(
            length(x[iUse * ind]),
            es$len
          ) |> errorfun()
          
          enumerate <- enumerate + 1L
        }
          
      }
    }
  }
}

# error checks - out of bounds ====
x <- 1:10

for(iUse in c(1, -1)) {
  
  expect_error(
    eval_stride(stride_ptrn(1, 20, c(TRUE, FALSE, FALSE, TRUE)), x, iUse),
    pattern = '`stride` out of bounds'
  ) |> errorfun()
  
  
  expect_error(
    eval_stride(stride_ptrn(20, 1, c(TRUE, FALSE, FALSE, TRUE)), x, iUse),
    pattern = '`stride` out of bounds'
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}


