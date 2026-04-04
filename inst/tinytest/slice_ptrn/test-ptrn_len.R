
# set-up ====
enumerate <- 0
sys.source(file.path(getwd(), "source", "functions4testing.R"), envir = environment())

list_fromto <- list(
  1, 100, 2, 99, 10, 90, 100, 1, 99, 2, 98, 3
)
list_patternlen <- list(
  2, 3, 4, 100
)
n <- length(list_fromto) * length(list_fromto) * length(list_patternlen)


# main tests ====

expected <- out <- vector("list", n)
counter <- 1L

for(iF in seq_along(list_fromto)) {
  for(iT in seq_along(list_fromto)) {
    for(iB in seq_along(list_patternlen)) {
      
      from = list_fromto[[iF]]
      to = list_fromto[[iT]]
      by = list_patternlen[[iB]]
      pattern = rep(c(TRUE, FALSE), ceiling(by/2)) |> sample(by)
      if(length(pattern) <= length(from:to)) {
        
        expected[[counter]] <- (from:to)[pattern] |> length()
        out[[counter]] <- ptrn_len(from, to, pattern)
        
        counter <- counter + 1L
        enumerate <- enumerate + 1L
      }
    }
  }
}

expect_equal(
  expected, out
)

