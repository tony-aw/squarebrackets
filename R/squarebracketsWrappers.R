
#' @keywords Internal
#' @noRd
.C_any_badindx <- function(x, val) {
  .Call("C_any_badindx", x = x, val = val)
}

#' @keywords Internal
#' @noRd
.C_any_nonpos <- function(x, val) {
  .Call("C_any_nonpos", x = x, val = val)
}
