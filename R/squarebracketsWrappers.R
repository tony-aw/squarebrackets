
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
.C_any_nonNULL <- function(x) {
  .Call("C_any_nonNULL", x = x)
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
.C_sub2ind_16d_32 <- function(sub, dimcumprod) {
  .Call("C_sub2ind_16d_32", sub = sub, dimcumprod = dimcumprod)
}

#' @keywords Internal
#' @noRd
.C_sub2ind_16d_64 <- function(sub, dimcumprod) {
  .Call("C_sub2ind_16d_64", sub = sub, dimcumprod = dimcumprod)
}


#' @keywords Internal
#' @noRd
.C_match_range <- function(o, m) {
  .Call("C_match_range", o = o, m = m)
}

