#' List or Lock All Currently Existing Bindings Pointing To Same Address
#'
#' @description
#' `sb_currentBindings(x, action = "list")` \cr
#' lists all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment. \cr
#' \cr
#' `sb_currentBindings(x, action = "checklock")` \cr
#' searches all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment,
#' and reports which of these are locked and which are not locked. \cr
#' \cr
#' `sb_currentBindings(x, action = "lockbindings")` \cr
#' searches all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment,
#' and locks them using \link[base]{lockBinding}. \cr
#' \cr
#' See also \link{squarebrackets_PassByReference} for information regarding
#' the relation between locked bindings and pass-by-reference modifications. \cr \cr
#' 
#' 
#' @param x the existing variable whose address to use when searching for bindings.
#' @param action a single string, giving the action to perform. \cr
#' Must be one of the following: 
#'  * `"list"` (default).
#'  * `"checklock"`.
#'  * `"lockbindings"`.
#' @param env the environment where to look for objects. \cr
#' If `NULL` (default), the caller environment is used.
#' 
#' 
#' 
#' @returns
#' For `sb_currentBindings(x, action = "list")`: \cr
#' Returns a character vector. \cr
#' \cr
#' For `sb_currentBindings(x, action = "checklock")`: \cr
#' Returns a named logical vector. \cr
#' The names give the names of the bindings, \cr
#' and each associated value indicates whether the binding is
#' locked (`TRUE`) or not locked (`FALSE`). \cr
#' \cr
#' For `sb_currentBindings(x, action = "lockbindings")`: \cr
#' Returns VOID. It just locks the currently existing bindings. \cr
#' To unlock the bindings, remove the objects (see \link[base]{rm}). \cr \cr
#'
#'
#' @examples
#' 
#' x <- as.mutable_atomic(1:10)
#' y <- x
#' lockBinding("y", environment())
#' sb_currentBindings(x)
#' sb_currentBindings(x, "checklock") # only y is locked
#' 
#' 
#' # since only y is locked, we can still modify y through x by reference:
#' sb_set(x, i = 1, rp = -1)
#' print(y) # modified!
#' rm(list= c("y")) # clean up
#' 
#' 
#' # one can fix this by locking ALL bindings:
#' y <- x
#' sb_currentBindings(x, "lockbindings") # lock all
#' sb_currentBindings(x, "checklock") # all bindings are locked, including y
#' # the 'squarebrackets' package respects the lock of a binding,
#' # provided all bindings of an address are locked;
#' # so this will give an error, as it should:
#' tinytest::expect_error(
#'   sb_set(x, i = 1, rp = -1),
#'   pattern = "object is locked"
#' )
#' 
#' # creating a new variable will NOT automatically be locked:
#' z <- y # new variable; will not be locked!
#' sb_currentBindings(x, "checklock") # z is not locked
#' sb_currentBindings(x, "lockbindings") # we must re-run this
#' sb_currentBindings(x, "checklock") # now z is also locked
#' tinytest::expect_error( # now z is also protected
#'   sb_set(z, i = 1, rp = -1),
#'   pattern = "object is locked"
#' )
#' 
#' rm(list= c("x", "y", "z")) # clean up
#' 
#' 
#' 

#' @rdname sb_currentBindings
#' @export
sb_currentBindings <- function(x, action = "list", env = NULL) {
  
  if(is.null(env)) env <- parent.frame(n = 1)
  
  message(paste0("searching environment: "), .rcpp_address(env))

  if(length(action) != 1 || !is.character(action)) {
    stop("`action` must be a single string")
  }
  
  lst <- ls(envir = env, all.names = TRUE, sorted = FALSE)
  ref_address <- .rcpp_address(x)
  shared_obj <- .rcpp_list_bindings(ref_address, env, lst)
  
  if(action == "list") {
    return(shared_obj)
  }
  if(action == "checklock") {
    result <- as.logical(rep(NA, length(shared_obj)))
    names(result) <- shared_obj
    for(i in shared_obj) {
      result[i] <- bindingIsLocked(i, env)
    }
    return(result)
  }
  if(action == "lockbindings") {
    for(i in shared_obj) {
      lockBinding(i, env)
    }
    return(invisible(NULL))
  }
  
  stop("unknown `action` specified")
  
}


#' @keywords internal
#' @noRd
.address_in_pkgs <- function(ref_address, pkgs) {
  for(i in pkgs) {
    ns <- loadNamespace(i)
    lst <- setdiff(
      ls(envir = ns, all.names = TRUE, sorted = FALSE),
      utils::lsf.str(envir = ns, all.names = TRUE)
    )
    check <- .rcpp_address_in_env(ref_address, ns, lst)
    if(check) {
      return(TRUE)
    }
  }
  return(FALSE)
  
}
