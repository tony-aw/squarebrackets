#' List or Lock All Currently Existing Bindings Pointing To Same Address
#'
#' @description
#' `currentBindings(x, action = "list")` \cr
#' lists all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment. \cr
#' \cr
#' `currentBindings(x, action = "checklock")` \cr
#' searches all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment,
#' and reports which of these are locked and which are not locked. \cr
#' \cr
#' `currentBindings(x, action = "lockbindings")` \cr
#' searches all \bold{currently existing} objects
#' sharing the same \bold{address} as `x`, in a given environment,
#' and locks them using \link[base]{lockBinding}. \cr
#' \cr
#' See also \link{squarebrackets_PassByReference} for information regarding
#' the relation between locked bindings and pass-by-reference modifications. \cr
#' \cr \cr
#' 
#' 
#' @param x the existing variable whose address to use when searching for bindings.
#' @param action a single string, giving the action to perform. \cr
#' Must be one of the following: 
#'  * `"list"` (default).
#'  * `"checklock"`.
#'  * `"lockbindings"`.
#' @param env the environment where to look for objects. \cr
#' If `NULL` (default), the caller environment is used. \cr \cr
#' 
#' 
#' @details
#' 
#' The \link[base]{lockBinding} function locks a binding of an object,
#' preventing modification. \cr
#' 'R' also uses locked bindings to prevent modification of objects from package namespaces. \cr
#' The pass-by-reference semantics of 'squarebrackets' in principle respect this,
#' and disallows modification of objects by reference. \cr
#' \cr
#' However, \link[base]{lockBinding} does not lock the address/pointer of an object,
#' only one particular binding of an object. \cr
#' This problematic; consider the following example: \cr
#' 
#' ```{r eval = FALSE}
#' x <- mutable_atomic(1:16)
#' y <- x
#' lockBinding("y", environment())
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' 
#' In the above code, `x` and `y` share the same address,
#' thus pointing to the same memory,
#' yet only `y` is actually locked. \cr
#' Since `x` is not locked, modifying `x` is allowed. \cr
#' But since `sb_set()`/`sb2_set()` performs modification by reference,
#' `y` will still be modified, despite being locked. \cr
#' \cr
#' The `currentBindings()` function
#' allows to user to:
#' find all \bold{currently existing} bindings in the \bold{caller environment}
#' sharing the same address as `x`,
#' and locking all these bindings. \cr
#' \cr
#' 
#' @section Warning: 
#' The `currentBindings()` function
#' only locks \bold{currently existing} bindings
#' in the \bold{specified environment}; \cr
#' bindings that are created \bold{after} calling `currentBindings()`
#' will not automatically be locked. \cr
#' Thus, every time the user creates a new binding of the same object,
#' and the user wishes it to be locked,
#' `currentBindings()` must be called again. \cr \cr
#' 
#' 
#' @returns
#' For `currentBindings(x, action = "list")`: \cr
#' Returns a character vector. \cr
#' \cr
#' For `currentBindings(x, action = "checklock")`: \cr
#' Returns a named logical vector. \cr
#' The names give the names of the bindings, \cr
#' and each associated value indicates whether the binding is
#' locked (`TRUE`) or not locked (`FALSE`). \cr
#' \cr
#' For `currentBindings(x, action = "lockbindings")`: \cr
#' Returns VOID. It just locks the currently existing bindings. \cr
#' To unlock the bindings, remove the objects (see \link[base]{rm}). \cr \cr
#'
#'
#' @example inst/examples/generic_currentBindings.R
#' 
#' 
#' 

#' @rdname currentBindings
#' @export
currentBindings <- function(x, action = "list", env = NULL) {
  
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
