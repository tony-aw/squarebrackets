
#' @keywords Internal
#' @noRd
.C_is_altrep <- function(x) {
  .Call("C_is_altrep", x = x)
}


#' @keywords Internal
#' @noRd
.C_any_badindx <- function(x, val) {
  .Call("C_any_badindx", x = x, val = val)
}

#' @keywords Internal
#' @noRd
.C_any_badmargin <- function(x, val) {
  .Call("C_any_badmargin", x = x, val = val)
}


#' @keywords Internal
#' @noRd
.C_any_nonpos <- function(x) {
  .Call("C_any_nonpos", x = x)
}

#' @keywords Internal
#' @noRd
.C_any_neg <- function(x) {
  .Call("C_any_neg", x = x)
}


#' @keywords Internal
#' @noRd
.C_convert_cplx_32 <- function(x, val) {
  .Call("C_convert_cplx_32", x = x, val = as.integer(val))
}


#' @keywords Internal
#' @noRd
.C_convert_cplx_64 <- function(x, val) {
  .Call("C_convert_cplx_64", x = x, val = as.double(val))
}


#' @keywords Internal
#' @noRd
.C_convert_cplx <- function(x, val) {
  
  if(is.integer(val)) {
    return(.C_convert_cplx_32(x, val))
  }
  if(is.double(val)) {
    return(.C_convert_cplx_64(x, val))
  }
}


#' @keywords Internal
#' @noRd
.C_match_range <- function(o, m) {
  .Call("C_match_range", o = o, m = m)
}


#' @keywords Internal
#' @noRd
.C_serial <- function(x) {
  .Call("C_serial", x = x)
}

#' @keywords Internal
#' @noRd
.C_any_address <- function(x, v) {
  .Call("C_any_address", x = x, v = v)
}

#' @keywords Internal
#' @noRd
.C_copy <- function(x) {
  .Call("C_copy", x = x)
}
