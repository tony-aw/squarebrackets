
# indices ====
x.original <- data.frame(
  a = -sample.int(10),
  b = letters[1:10],
  c = rnorm(10),
  d = rep(c(TRUE, FALSE), 5)
)
row <- indx_named(x.original, 1)
col <- indx_named(x.original, 2)
filter <- list(
  NULL,
  ~ (a >= 4) & (b != "a") & (d==TRUE), # partially TRUE/FALSE
  ~ (a > 10) | (b == "z"), # completely FALSE
  ~ (a < 10) | (b == "a") # completely TRUE
)
get_vars <- list(
  NULL,
  is.numeric, # partially TRUE/FALSE
  is.factor, # completely FALSE
  is.atomic # completely TRUE
)


if(!test_PassByReference) {
  # data.frame ====
  x <- as.data.frame(x.original)
  
  temp.fun.main(x, row, col, filter, get_vars)
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
  
  # tibble ====
  if(requireNamespace("tibble")) {
    x <- tibble::as_tibble(x.original)
    
    temp.fun.main(x, row, col, filter, get_vars)
    
    
    if(isTRUE(test_allow_duplicates)) {
      expect_equal(
        anyDuplicated(colnames(sb_x(x, col = c(1,1,1)))),
        0
      )
      expect_equal(
        anyDuplicated(colnames(sb_x(x, col = c("a","a","a")))),
        0
      )
      enumerate <- enumerate <- enumerate + 1
    }
  }
  
  
}


# data.table ====
if(requireNamespace("data.table")) {
  
  x <- dt.$as.data.table(x.original)
  temp.fun.main(x, row, col, filter, get_vars)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}





# tidytable ====
if(requireNamespace("tidytable")) {
  x <- tidytable::as_tidytable(x.original)
  
  temp.fun.main(x, row, col, filter, get_vars)
  
  
  if(isTRUE(test_allow_duplicates)) {
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c(1,1,1)))),
      0
    )
    expect_equal(
      anyDuplicated(colnames(sb_x(x, col = c("a","a","a")))),
      0
    )
    enumerate <- enumerate <- enumerate + 1
  }
  
}
