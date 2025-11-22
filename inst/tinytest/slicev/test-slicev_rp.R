
# setup ====

enumerate <- 0L
errorfun <- function(tt) {
  if(isTRUE(tt)) print(tt)
  if(isFALSE(tt)) stop(print(tt))
}


basetest_single <- function(x, ..., y, v, r, na) {
  
  rp <- parent.frame()$rp
  
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
  
  x[ind] <- rp
  return(x)
}



basetest_numrng <- function(x, ..., y, v, r, na) {
  
  rp <- parent.frame()$rp
  
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
  
  x[ind] <- rp
  return(x)
}


basetest_str <- function(x, ..., y, v, r, na) {
  
  rp <- parent.frame()$rp
  
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
  
  x[ind] <- rp
  return(x)
}


slicetest <- function(x, ..., y, v, r, na) {
  
  x <- data.table::copy(x)
  x2 <- x
  
  rp <- parent.frame()$rp
  
  slicev_set(x, y = y, v = v, r = r, na = na, rp = rp)
  if(!identical(x, x2)) { stop("PassByReference fail")}
  return(x)
}

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


