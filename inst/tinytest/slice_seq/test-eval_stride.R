
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())


# vector checks ====
x <- 1:10
for(iFrom in 1:10) {
  for(iTo in 1:10) {
    for(iBy in 1:10) {
      for(iUse in c(-1, 1)) {
        
        from <- iFrom
        to <- iTo
        by <- ifelse(from > to, -iBy, iBy)
        
        stride <- stride_seq(from, to, by)
        ind <- seq(from, to, by)
        es <- eval_stride(stride, x, iUse)
        
        if(iUse < 0) {
          expect_equal(
            range(ind), # because order is irrelevant when inverting
          as.numeric(es[1:2])
          ) |> errorfun()
        }
        else {
          expect_equal(
            c(ind[1], ind[length(ind)]),
          as.numeric(es[1:2])
          ) |> errorfun()
        }
        
        expect_equal(
          length(x[iUse * ind]),
          es$len
        ) |> errorfun()
        
        enumerate <- enumerate + 2L
        
      }
    }
  }
}

# error checks - out of bounds ====
x <- 1:10

for(iUse in c(1, -1)) {
  
  expect_error(
    eval_stride(stride_seq(1, 20, 1), x, iUse),
    pattern = '`stride` out of bounds'
  ) |> errorfun()
  
  
  expect_error(
    eval_stride(stride_seq(20, 1, 1), x, iUse),
    pattern = '`stride` out of bounds'
  ) |> errorfun()
  
  enumerate <- enumerate + 2L
  
}


