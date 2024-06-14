errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}

any_empty_indices <- function(...) {
  lst <- list(...)
  return(squarebrackets:::.any_empty_indices(lst))
}