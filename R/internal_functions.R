#

#' @keywords internal
#' @noRd
.check_bindingIsLocked <- function(subx, env, abortcall) {
  if(!is.symbol(subx)) {
    stop(simpleError(
      "only existing variables can be modified by reference", call = abortcall
      ))
  }
  if(bindingIsLocked(subx, env = env)){
    stop(simpleError("object binding is locked", call = abortcall))
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
.indx_make_filter <- function(x, filter, inv, abortcall) {
  
  if(length(filter) != 2L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  
  mm <- .with_internal(x, filter, abortcall)
  environment(filter) <- NULL
  
  if(!is.logical(mm)) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  if(!inv)return(which(mm))
  if(inv)return(which(!mm))
  
}

#' @keywords internal
#' @noRd
.indx_make_vars_range <- function(x, form, inv, abortcall) {
  
  if(length(form) != 3L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  vars <- all.vars(form)
  if(length(vars) != 2L) {
    stop(simpleError("improper formula given", call = abortcall))
  }
  nms <- names(x)
  pos1 <- .rcpp_dt_find_name(nms, vars[1L], 1L)
  pos2 <- .rcpp_dt_find_name(nms, vars[2L], 1L)
  
  rng <- pos1:pos2
  if(inv) {
    rng <- seq_len(length(x))[-rng]
  }
  return(rng)
}

#' @keywords internal
#' @noRd
.indx_make_vars <- function(x, vars, inv, abortcall) {
  if(!is.function(vars)) {
    stop(simpleError("`vars` must be a function", call = abortcall))
  }
  out <- collapse::get_vars(x, vars, return = "logical")
  if(!inv)return(which(out))
  if(inv)return(which(!out))
}



#' @keywords internal
#' @noRd
.check_args_array <- function(x, s, d, abortcall) {
  
  dims_explicit <- length(d) != ndim(x) && length(d) != 0L
  
  if(dims_explicit && is.null(s)) {
    stop(simpleError("improper combination of `s` and `d` specified"))
  }
  
}



#' @keywords internal
#' @noRd
.check_args_df <- function(x, s, d, obs, vars, abortcall) {
  
  used_sd <- !is.null(s)
  used_obsvars <- !is.null(obs) || !is.null(vars)
  
  if(used_sd && used_obsvars) {
    stop(simpleError(
      "cannot specify the `s, d` arguments and `obs, vars` arguments simultaneously",
      call = abortcall
    ))
  }
  # if(collapse::any_duplicated(names(x))) {
  #   stop(simpleError(
  #     "`x` does not have unique variable names for all columns; \n fix this before subsetting",
  #     call = abortcall
  #   ))
  # }
}


#' @keywords internal
#' @noRd
.any_empty_indices <- function(lst) {
  check <- vapply(lst, \(x)!is.null(x) && length(x) == 0L, FUN.VALUE = logical(1L))
  if(any(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


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
.internal_check_rptf <- function(rp, tf, abortcall) {
  if(!missing(rp) && !missing(tf)) {
    stop(simpleError("cannot specify both `rp` and `tf`", call = abortcall))
  }
  if(missing(rp) && missing(tf)) {
    stop(simpleError("must specify either `rp` or `tf`", call = abortcall))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) {
      stop("`tf` must be a function")
    }
  }
}

#' @keywords internal
#' @noRd
.check_rp_atomic <- function(rp, sslength, abortcall) {
  n.rp <- length(rp)
  if(!is.atomic(rp)) {
    stop(simpleError("replacement must be atomic", call = abortcall))
  }
  if(n.rp != sslength && n.rp != 1L) {
    stop(simpleError("recycling not allowed", call = abortcall))
  }
  # if(typeof(rp) != sstype) stop("type coercion not allowed")
}



#' @keywords internal
#' @noRd
.check_rp_list <- function(rp, sslength, abortcall) {
  n.rp <- length(rp)
  if(!is.list(rp)) {
    stop(simpleError("replacement must be a list", call = abortcall))
  }
  if(sslength != n.rp && n.rp != 1L) {
    stop(simpleError("recycling not allowed", call = abortcall))
  }
  # if(any(collapse::vtypes(rp) != sstypes)) stop("type coercion not allowed")
}

#' @keywords internal
#' @noRd
.with_internal <- function(data, form, abortcall) {
  vars <- all.vars(form)
  env <- environment(form)
  search_names <- c(names(data), names(env))
  if(any(!vars %in% search_names)) stop("unknown variable(s) given")
  txt <- as.character(form)[2L]
  out <- eval(parse(text = txt), data, enclos = env)
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
.internal_is_formula <- function(form) {
  check <- inherits(form, "formula") && is.call(form) && isTRUE(form[[1]] == "~")
  return(check)
}


#' @keywords internal
#' @noRd
.internal_coerce_rp <- function(x, rp, abortcall) {
  rp_na <- length(rp) == 1L && is.na(rp)
  if(!rp_na && typeof(x) != typeof(rp)) {
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
