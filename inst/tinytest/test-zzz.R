
tempfun <- function(x) {
  if(!is.function(x)) {
    return(squarebrackets:::.rcpp_address(x))
  }
}

lst <- eapply(baseenv(), tempfun, all.names = TRUE, USE.NAMES = TRUE)
lst <- lst[sapply(lst, \(x)!is.null(x))]
protected_bnds <- sapply(
  names(lst), \(x) bindingIsLocked(x, env = baseenv()) || bindingIsActive(x, env = baseenv())
)
lst <- lst[protected_bnds]
lst <- lst[!names(lst) %in% c(".Last.value", "Last.value")]

expected <- sort(unlist(lst))
outcome <- sort(squarebrackets:::.protected_addresses())
setdiff(outcome, expected)

expect_equal(
  expected,
  outcome
)

outcome <-  sort(getOption("squarebrackets.protected"))
setdiff(outcome, expected)

expect_equal(
  expected,
  outcome 
)
