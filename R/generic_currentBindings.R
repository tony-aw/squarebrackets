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
#' @example inst/examples/generic_currentBindings.R
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
