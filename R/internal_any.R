
#' @keywords Internal
#' @noRd
.any_badindx <- function(x, val) {
  if(.C_is_altrep(x)) {
    r <- c(x[1], x[length(x)])
    return(any(r < 1 | r > val))
  }
  else if(is.integer(x) && val > (2^31 - 1L)) {
    .C_any_nonpos(x)
  }
  else {
    .C_any_badindx(x, val)
  }

}

#' @keywords Internal
#' @noRd
.any_badmargin <- function(x, val) {
  if(.C_is_altrep(x)) {
    r <- c(x[1], x[length(x)])
    return(any(r < 0 | r > val))
  }
  else if(is.integer(x) && val > (2^31 - 1L)) {
    .C_any_neg(x)
  }
  else {
    .C_any_badmargin(x, val)
  }
}


#' @keywords Internal
#' @noRd
.any_nonpos <- function(x) {
  if(.C_is_altrep(x)) {
    r <- c(x[1], x[length(x)])
    return(any(r < 1))
  }
  else {
    .C_any_nonpos(x = x)
  }
}

#' @keywords Internal
#' @noRd
.any_neg <- function(x) {
  if(.C_is_altrep(x)) {
    r <- c(x[1], x[length(x)])
    return(any(r < 0))
  }
  else {
    .C_any_neg(x = x)
  }

}
