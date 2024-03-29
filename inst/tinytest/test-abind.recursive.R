
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
  dimnames1 <- list(letters[1:4], rev(letters[1:4]), letters[1:4])
  dimnames2 <- list(letters[5:8], rev(letters[5:8]), letters[5:8])
  dimnames2[[margin]] <- "z"
  ya <- array(1:64, c(4,4,4))
  yl <- array(as.list(1:64), c(4,4,4))
  newa <- array(1:16, dim = dims)
  newl <- array(as.list(1:16), dim = dims)
  dimnames(ya) <- dimnames1
  dimnames(yl) <- dimnames1
  dimnames(newa) <- dimnames2
  dimnames(newl) <- dimnames2
  
  out <- .abind.recursive(list(yl, newl, yl), margin)
  pre_expected <- abind::abind(ya, newa, ya, along = margin)
  expected <- array(
    as.list(pre_expected),
    dim = dim(pre_expected),
    dimnames = dimnames(pre_expected)
  )
  expect_equal(
    out, expected
  ) |> errorfun()
  
  
  enumerate <- enumerate + 1
}
