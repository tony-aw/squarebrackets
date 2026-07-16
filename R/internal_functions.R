#

#' @keywords internal
#' @noRd
.check_bindingIsLocked <- function(sym, env, abortcall) {
  if(!is.symbol(sym)) {
    stop(simpleError(
      "only objects that exist as variables can be modified by reference", call = abortcall
      ))
  }
  if(bindingIsLocked(sym, env = env)){
    txt <- paste0("cannot change value of locked binding for '", sym, "'")
    stop(simpleError(txt, call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_stop <- function(abortcall) {
  stop(simpleError(
    "incorrect index type",
    call = abortcall
  ))
}


#' @keywords internal
#' @noRd
.check_args_array <- function(x, s, use, abortcall) {
  
  if(!is.list(s) && !is.formula(s) && !is.atomic(s) && !is.null(s)) {
    stop(simpleError("improper `s` specified", call = abortcall))
  }
  
}



# 
# .check_args_df <- function(x, s, d, obs, vars, abortcall) {
#   
#   used_sd <- !is.null(s)
#   used_obsvars <- !is.null(obs) || !is.null(vars)
#   
#   if(used_sd && used_obsvars) {
#     stop(simpleError(
#       "cannot specify the `s, d` arguments and `obs, vars` arguments simultaneously",
#       call = abortcall
#     ))
#   }
#   # if(collapse::any_duplicated(names(x))) {
#   #   stop(simpleError(
#   #     "`x` does not have unique variable names for all columns; \n fix this before subsetting",
#   #     call = abortcall
#   #   ))
#   # }
# }




#' @keywords internal
#' @noRd
.internal_fix_names <- function(x, f) {
  x.names <- names(x)
  dim(x.names) <- dim(x)
  x <- f(x)
  names(x) <- f(x.names)
  return(x)
}


#' @keywords internal
#' @noRd
.check_rp <- function(x, rp, sslength, abortcall) {
  n.rp <- length(rp)
  if(is.atomic(rp) != is.atomic(x) || is.list(rp) != is.list(x)) {
    stop(simpleError("replacement must match `is.atomic(x)` and `is.list(x)`", call = abortcall))
  }
  if(n.rp != sslength && n.rp != 1L) {
    stop(simpleError("recycling not allowed", call = abortcall))
  }
  # if(typeof(rp) != sstype) stop("type coercion not allowed")
}



#' @keywords internal
#' @noRd
.with_internal <- function(data, form, abortcall) {
  vars <- all.vars(form)
  env <- environment(form)
  search_names <- c(names(data), names(env))
  missing.vars <- vars[!vars %in% search_names]
  if(any(!vars %in% search_names)) {
    txt <- paste0("unknown variable(s) given: \n", missing.vars)
    stop(simpleError("unknown variable(s) given", call = abortcall))
  }
  txt <- as.character(form)[2L]
  out <- eval(parse(text = txt), data, enclos = env)
  environment(form) <- NULL
  return(out)
}

#' @keywords internal
#' @noRd
.with_array <- function(x, keywords, form, abortcall) {
  if(!is.formula(form)) {
    stop(simpleError("invalid formula given"))
  }
  if(length(form) != 2L) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  env <- environment(form)
  txt <- as.character(form)[2L]
  out <- eval(parse(text = txt), keywords, enclos = env)
  environment(form) <- NULL
  return(out)
}

#' @keywords internal
#' @noRd
.internal_check_dots <- function(dots.list, abortcall) {
  # this check will not take much performance
  if(length(dots.list) > 0L) {
    if(!is.null(names(dots.list))) {
      error.txt <- paste0(
        "unknown arguments given:",
        "\n",
        paste(names(dots.list), collapse = ", ")
      )
    }
    else {
      error.txt <- paste0(
        "unknown arguments given (unnamed)"
      )
    }
    stop(simpleError(error.txt, call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.internal_coerce_rp <- function(x, rp, abortcall) {
  if(typeof(rp) != typeof(x)) {
    message(sprintf("coercing replacement to %s", typeof(x)))
    if(is.logical(x)) rp <- as.logical(rp)
    else if(is.integer(x)) rp <- as.integer(rp)
    else if(is.double(x)) rp <- as.double(rp)
    else if(is.complex(x)) rp <- as.complex(rp)
    else if(is.character(x)) rp <- as.character(rp)
    else if(is.raw(x)) rp <- as.raw(rp)
    else {
      stop(simpleError(
        "unsupported atomic type", call = abortcall
      ))
    }
  }
  return(rp)
  
}


#' @keywords internal
#' @noRd
.internal_make_use_tabular <- function(use, abortcall) {
  
  if(length(use) == 1L) {
    if(abs(use) == 1L) {
      return(c(use, 2L))
    }
    else if(abs(use) == 2L) {
      return(c(1L, use))
    }
    else {
      stop(simpleError("improper `use` specified", call = abortcall))
    }
  }
  else if(length(use) == 2L) {
    if(.C_any_baduse(use, 2L) || is.unsorted(abs(use))) {
      stop(simpleError("improper `use` specified", call = abortcall))
    }
    if(anyDuplicated(use)) {
      stop(simpleError("`use` cannot have duplicate values", call = abortcall))
    }
    return(use)
  }
  else {
    stop(simpleError("`use` must be of length 1 or 2", call = abortcall))
  }
  
}


.is.multiple <- function(len1, len2) {
  (max(len1, len2) %% min(len1, len2)) == 0
}
