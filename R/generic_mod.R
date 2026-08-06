#' Methods to Replace Subsets using R's Native Modification Semantics
#'
#' @description
#' Methods to replace subsets. \cr
#' Atomic objects are modified using R's native Modification semantics. \cr
#' Recursive objects are modified via a careful shallow (not deep) copy. \cr
#' \cr
#'
#' @param x see \link{squarebrackets_supported_structures}.
#' @param i,use,s,row,col See \link{squarebrackets_index_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... see \link{squarebrackets_ellipsis}.
#' @param rp,tf,env see \link{squarebrackets_modify}.
#' @param chkdup see \link{squarebrackets_options}. \cr
#' `r .mybadge_performance_set2("FALSE")`
#' 
#' 
#' 
#' @details
#' \bold{Method Dispatch} \cr
#' Method dispatching is handled primarily through R's own `[<-` method dispatch. \cr
#' The exception is data.frames, wh
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' \cr
#' 
#' @returns
#' Nothing. The object is modified in-place as-if running `x[...] <- value` \cr
#' \cr
#'
#' @concept _mod
#' @example inst/examples/generic_mod.R
#' 
#' @name generic_mod
NULL




#' @rdname generic_mod
#' @export
ii_mod <- function(
    x, i = NULL, use = 1, ...,
    rp, tf, env = NULL, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  if(is.null(env)) env <- parent.frame()
  .internal_check_dots(list(...), sys.call())
  x_expr <- substitute(x)
  
  # evaluate if x is atomic; do in parent.frame to avoid R's quick-draw semantics
  x_is_atomic  <- eval(call("is.atomic", x_expr), envir = env)
  
  if(x_is_atomic) {
    # x is atomic;
    # use its shadow to avoid R's quick-draw semantics
    x_shadow <- cast_ArrayShadow2(
      x_expr, env
    )
  }
  else {
    x_shadow <- x
    if(!missing(tf)) tf <- .funply(tf)
  }
  
  
  # eval methodcheck & ci_ss on shadow:
  .methodcheck.ii(x_shadow, sys.call())
  .argscheck_rptf(rp, tf, sys.call())
  
  .flat_mod(x_expr, x_shadow, env, i, use, chkdup, rp, tf, sys.call())
  
  return(invisible(NULL))
}

#' @rdname generic_mod
#' @export
ss_mod <- function(
    x, s = NULL, use = Inf, ...,
    rp, tf, env = NULL, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  
  if(is.null(env)) env <- parent.frame()
  .internal_check_dots(list(...), sys.call())
  x_expr <- substitute(x)
  x_shadow <- cast_ArrayShadow2(
    x_expr, env
  )
  .methodcheck.ss(x_shadow, sys.call())
  .argscheck_rptf(rp, tf, sys.call())
  
  .arr_mod(x_expr, x_shadow, env, s, use, chkdup, rp, tf, sys.call())
  return(invisible(NULL))
}



#' @rdname generic_mod
#' @export
tt_mod <- function(
    x, row = NULL, col = NULL, use = 1:2, ...,
    rp, tf, env = NULL, chkdup = getOption("squarebrackets.chkdup", FALSE)
) {
  
  if(is.null(env)) env <- parent.frame()
  .internal_check_dots(list(...), sys.call())
  x_expr <- substitute(x)
  
  x_shadow <- cast_ArrayShadow2(
    x_expr, env
  )
  
  if(!is.data.frame(x_shadow)) {
    .methodcheck.tt(x_shadow, sys.call())
    .argscheck_rptf(rp, tf, sys.call())
    use <- .internal_make_use_tabular(use, sys.call())
    .arr_mod(x_expr, x_shadow, env, n(row, col), use, chkdup, rp, tf, sys.call())
    return(invisible(NULL))
  }
  else {
    .methodcheck.tt(x, sys.call())
    .argscheck_rptf(rp, tf, sys.call())
    # checks:
    .internal_check_dots(list(...), sys.call())
    
    if(length(x) == 0L) {
      return(invisible(NULL))
    }
    
    # make arguments:
    rowcol <- ci_df(x, row, col, use, chkdup, sys.call())
    row <- rowcol[[1L]]
    col <- rowcol[[2L]]
    
    # empty indices:
    if(.any_empty_indices(n(row, col))) {
      return(invisible(NULL))
    }
    
    # prep col:
    if(.C_is_missing_idx(col)) {
      message("copying all columns")
      col <- seq_len(ncol(x))
    }
    
    # make shallow copy of x as a whole, and deep copy of the columns to modify:
    x <- collapse::ftransformv(x, col, data.table::copy, apply = TRUE)
    
    # prep replacement just in case:
    if(!missing(rp)) {
      rp <- .dt_prep_rp(rp)
    }
    
    # tramsformation:
    if(!missing(tf)) {
      tf <- .funply(tf)
      rp <- .dt_transform(x, row, col, tf)
    }
    
    if(.C_is_missing_idx(row)) row <- seq_len(nrow(x))
    
    # modify:
    assign(
      as.character(x_expr), .dt_mod(x, row, col, rp, sys.call()), envir = env
    )
    return(invisible(NULL))
  }
  
}

