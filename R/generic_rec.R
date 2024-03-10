#' Access Recursive Subsets
#'
#' @description
#' The `sb_rec()` method allows the user to access recursive subsets of lists. \cr
#' \cr
#' The `sb_rec()` method also allows replacing or transforming a recursive subset of a list,
#' using R's default in-place semantics,
#' by specifying the `rp` argument.
#' 
#' @param lst a list, or list-like object.
#' @param rec a vector of length `p`,
#' such that `lst[[rec]]` is equivalent to `lst[[ rec[1] ]]...[[ rec[p] ]]`,
#' providing all but the final indexing results in a list. \cr
#' When on a certain subset level of a nested list,
#' multiple subsets with the same name exist,
#' only the first one will be selected when performing recursive indexing by name,
#' due to the recursive nature of this type of subsetting.
#' @param rp optional. If specified, performs `lst[[rec]] <- rp`,
#' using R's default in-place semantics. \cr \cr
#' 
#'
#' @returns
#' If `rp` is not specified: Returns the recursive subset. \cr
#' If `rp` is specified: Returns nothing,
#' but replaces the recursive subset with `rp`,
#' using R's default in-place semantics. \cr \cr
#' 
#'
#'
#'
#' @examples
#' 
#' lst <- list(
#'   A = list(
#'     A = list(A = "AAA", B = "AAB"),
#'     A = list(A  = "AA2A", B = "AA2B"),
#'     B = list(A = "ABA", B = "ABB")
#'   ),
#'   B = list(
#'     A = list(A = "BAA", B = "BAB"),
#'     B = list(A = "BBA", B = "BBB")
#'   )
#' )
#' 
#' #############################################################################
#' 
#' # access recursive subsets ====
#' 
#' sb_rec(lst, c(1,2,2)) # this gives "AA2B"
#' sb_rec(lst, c("A", "B", "B")) # this gives "ABB"
#' sb_rec(lst, c(2,2,1)) # this gives "BBA"
#' sb_rec(lst, c("B", "B", "A")) # this gives "BBA"
#' 
#' 
#' #############################################################################
#' 
#' # replace with R's default in-place semantics ====
#' 
#' # replace "AAB" using R's default in-place semantics:
#' sb_rec(
#'   lst, c("A", "A", "B"),
#'   rp = "THIS IS REPLACED WITH IN-PLACE SEMANTICS"
#' )
#' print(lst)
#' 
#'
#' #############################################################################
#' 
#' # Modify View of List By Reference ====
#' 
#' x <- list(
#'  a = data.table::data.table(cola = 1:10, colb = letters[1:10]),
#'  b = data.table::data.table(cola = 11:20, colb = letters[11:20])
#' )
#' print(x)
#' mypointer <- sb_rec(x, "a")
#' address(mypointer) == address(x$a) # they are the same
#' sb_set(mypointer, col = "cola", tf = \(x)x^2)
#' print(x) # notice x has been changed
#' 
#'

#' @name sb_rec
NULL



#' @rdname sb_rec
#' @export
sb_rec <- function(lst, rec, rp) {
  
  if(!is.list(lst)) {
    stop("`lst` must be a list")
  }
  
  if(missing(rp)) {
    return(lst[[rec]])
  }
  
  if(!missing(rp)) {
    eval.parent(substitute(lst[[rec]] <- rp))
  }
  
}



