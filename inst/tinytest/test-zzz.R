
tempfun <- function(x) {
  if(!is.function(x)) {
    return(squarebrackets:::.rcpp_address(x))
  }
}
lst <- eapply(baseenv(), tempfun)
lst <- lst[sapply(lst, \(x)!is.null(x))]
expected <- sort(unlist(lst))
outcome <- sort(squarebrackets:::.protected_addresses())
setdiff(outcome, expected)
expect_equal(
  expected,
  outcome
)
expect_equal(
  expected,
  sort(getOption("squarebrackets.protected"))
)
