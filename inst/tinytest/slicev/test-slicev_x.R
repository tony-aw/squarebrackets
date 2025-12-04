
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


basetest_single <- function(x, ..., y, v, r, na) {
  
  cond <- (y == v) == r
  
  if(isTRUE(na)) {
    ind <- which(ifelse(is.na(y), TRUE, cond))
  }
  else if(isFALSE(na)) {
    ind <- which(cond)
  }
  else if(is.na(na)) {
    cond <- is.na(y) == r
    ind <- which(cond)
  }
  
  out <- x[ind]
  return(out)
}



basetest_numrng <- function(x, ..., y, v, r, na) {
  
  cond <- (y >= v[1] & y <= v[2]) == r
  
  if(isTRUE(na)) {
    ind <- which(ifelse(is.na(y), TRUE, cond))
  }
  else if(isFALSE(na)) {
    ind <- which(cond & !is.na(y))
  }
  else if(is.na(na)) {
    cond <- is.na(y) == r
    ind <- which(cond)
  }
  
  out <- x[ind]
  return(out)
}


basetest_str <- function(x, ..., y, v, r, na) {
  
  cond <- (y %in% v) == r
  
  if(isTRUE(na)) {
    ind <- which(ifelse(is.na(y), TRUE, cond))
  }
  else if(isFALSE(na)) {
    ind <- which(ifelse(is.na(y), FALSE, cond))
  }
  else if(is.na(na)) {
    cond <- is.na(y) == r
    ind <- which(cond)
  }
  
  out <- x[ind]
  return(out)
}


slicetest <- function(x, ..., y, v, r, na) {
  slicev_x(x, y = y, v = v, r = r, na = na)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


