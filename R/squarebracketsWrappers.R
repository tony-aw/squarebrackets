
#' @keywords Internal
#' @noRd
.C_is_altrep <- function(x) {
  .Call("C_is_altrep", x = x)
}

#' @keywords Internal
#' @noRd
.C_is_missing_idx <- function(x) {
  .Call("C_is_missing_idx", x = x)
}



#' @keywords Internal
#' @noRd
.C_any_badindx <- function(x, val) {
  .Call("C_any_badindx", x = x, val = val)
}

#' @keywords Internal
#' @noRd
.C_any_baduse <- function(x, val) {
  .Call("C_any_baduse", x = x, val = val)
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
.C_all_dim_zero <- function(x) {
  .Call("C_all_dim_zero", x = x)
}

#' @keywords Internal
#' @noRd
.C_convert_bi_32 <- function(x, val) {
  .Call("C_convert_bi_32", x = x, val = as.integer(val))
}


#' @keywords Internal
#' @noRd
.C_convert_bi_64 <- function(x, val) {
  .Call("C_convert_bi_64", x = x, val = as.double(val))
}


#' @keywords Internal
#' @noRd
.C_convert_bi <- function(x, size) {
  
  if(is.integer(size)) {
    return(.C_convert_bi_32(x, size))
  }
  if(is.double(size)) {
    return(.C_convert_bi_64(x, size))
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
