
#' @keywords Internal
#' @noRd
.C_any_badindx <- function(x, val) {
  if(is.double(val)) {
    x <- as.double(x)
  }
  if(is.integer(val)) {
    x <- as.integer(x)
  }
  .Call("C_any_badindx", x = x, val = val)
}

#' @keywords Internal
#' @noRd
.C_any_badmargin <- function(x, val) {
  if(is.double(val)) {
    x <- as.double(x)
  }
  if(is.integer(val)) {
    x <- as.integer(x)
  }
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
.C_sub2ind_2d <- function(
    ind1, ind2, dimcumprod
) {
  .Call(
    "C_sub2ind_2d",
    ind1 = as.integer(ind1), ind2 = as.integer(ind2),
    dimcumprod = as.double(dimcumprod)
  )
}

#' @keywords Internal
#' @noRd
.C_sub2ind_3d <- function(
    ind1, ind2, ind3, dimcumprod
) {
  .Call(
    "C_sub2ind_3d",
    ind1 = as.integer(ind1), ind2 = as.integer(ind2), ind3 = as.integer(ind3),
    dimcumprod = as.double(dimcumprod)
  )
}

#' @keywords Internal
#' @noRd
.C_sub2ind_4d <- function(
    ind1, ind2, ind3, ind4, dimcumprod
) {
  .Call(
    "C_sub2ind_4d",
    ind1 = as.integer(ind1), ind2 = as.integer(ind2), ind3 = as.integer(ind3), ind4 = as.integer(ind4),
    dimcumprod = as.double(dimcumprod)
  )
}

#' @keywords Internal
#' @noRd
.C_sub2ind_5d <- function(
    ind1, ind2, ind3, ind4, ind5, dimcumprod
) {
  .Call(
    "C_sub2ind_5d",
    ind1 = as.integer(ind1), ind2 = as.integer(ind2), ind3 = as.integer(ind3), ind4 = as.integer(ind4), ind5 = as.integer(ind5),
    dimcumprod = as.double(dimcumprod)
  )
}

#' @keywords Internal
#' @noRd
.C_sub2ind_6d <- function(
    ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod
) {
  .Call(
    "C_sub2ind_6d",
    ind1 = as.integer(ind1), ind2 = as.integer(ind2), ind3 = as.integer(ind3), ind4 = as.integer(ind4), ind5 = as.integer(ind5), ind6 = as.integer(ind6),
    dimcumprod = as.double(dimcumprod)
  )
}
