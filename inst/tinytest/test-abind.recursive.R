
# set-up ====

source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0
temp.fun <- function(x, ...) {
  return(x[...])
}
enumerate <- 0

.abind.recursive <- squarebrackets:::.abind.recursive

for(margin in 1:3) {
  
  dims <- c(4,4,4)
  dims[margin] <- 1
  ya <- array(1:64, c(4,4,4))
  yl <- array(as.list(1:64), c(4,4,4))
  newa <- array(1:16, dim = dims)
  newl <- array(as.list(1:16), dim = dims)
  
  out <- .abind.recursive(list(yl, newl, yl), margin)
  expected <- array(as.list(abind::abind(ya, newa, ya, along = margin)), dim = dim(out))
  expect_equivalent(
    out, expected
  ) |> errorfun()
  
  
  enumerate <- enumerate + 1
}
