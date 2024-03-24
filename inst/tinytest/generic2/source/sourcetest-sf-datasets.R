
# indices ====
x.sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
n <- nrow(x.sf)
x.extra <- data.frame(
  a = - sample.int(n),
  b = sample(letters, n, TRUE),
  c = rnorm(n),
  d = sample(c(TRUE, FALSE), n, TRUE)
)
x.original <- cbind(x.extra, x.sf)
row <- indx_general(x.original, 1)
col <- indx_general(x.original, 2)
filter <- list(
  NULL,
  ~ (a >= 4) & (b != "a") & (d==TRUE), # partially TRUE/FALSE
  ~ (a > 10) | (b == "z"), # completely FALSE
  ~ (a < 10) | (b == "a") # completely TRUE
)
get_vars <- list(
  NULL,
  is.double, # partially TRUE/FALSE
  is.factor, # completely FALSE
  is.atomic # completely TRUE
)

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
