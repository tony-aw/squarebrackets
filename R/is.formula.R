#' Check if an Object is a Formula
#'
#' @description
#' `is.formula()` checks if an object is a formula. \cr \cr
#' 
#' @param form object to check
#'
#' @returns
#' The `is_formula()` function returns `TRUE` if the input is a formula,
#' and `FALSE` otherwise. \cr \cr
#'
#'
#'
#' @examples
#' is.formula(~ x)
#' is.formula(1:10)
#' 



#' @rdname is.formula
#' @export
is.formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}
