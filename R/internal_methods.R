
#' @export
length.ArrayShadow <- function(x) {
  attr(x, "shadow_len")
}

#' @export
names.ArrayShadow <- function(x) {
  attr(x, "shadow_nms")
}

#' @export
dim.ArrayShadow <- function(x) {
  attr(x, "shadow_dim")
}

#' @export
dimnames.ArrayShadow <- function(x) {
  attr(x, "shadow_dimnames")
}
