
# set-up ====

source(file.path(getwd(), "source", "functions4testing.R"))
enumerate <- 0
temp.fun <- function(x, ...) {
  return(x[...])
}
enumerate <- 0


# test main ====
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
  
  out <- bind_array(list(yl, newl, yl), margin)
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

# test name_flat argument ====
x <- as.list(letters)
dim(x) <- c(2, 13)
names(x) <- LETTERS
y <- as.list(LETTERS)
dim(y) <- c(2, 13)
names(y) <- letters
z <- bind_array(list(x, y), 1, name_flat = TRUE)
expect_equal(
  as.character(tolower(z)), as.character(tolower(names(z)))
)
enumerate <- enumerate + 1

# test errors ====
x <- array(as.list(1:1e4), dim = c(100,50, 10))
y <- array(as.list(-1e4:-1), dim =c(100,10, 10))
expect_error(
  bind_array(list(x,y), along = 1),
  pattern = "non-conformable dimensions"
)
expect_error(
  bind_array(list(x,y), along = 3),
  pattern = "non-conformable dimensions"
)
enumerate <- enumerate + 2


