
#' @keywords internal
#' @noRd
.internal_abind <- function(
    arg.list, along, atomic, name_along, abortcall
) {
  N <- max(
    1L,
    vapply(arg.list, ndims, integer(1L))
  )
  
  extra_dimensional <- FALSE
  if(along == 0L || along > N) {
    extra_dimensional <- TRUE
  }
  
  ## N will eventually be length(dim(return.value))
  if (along < 1L || along > N || (along > floor(along) && along < ceiling(along))) {
    N <- N + 1L
    along <- max(1L, min(N + 1L, ceiling(along)))
  }
  
  ## this next check should be redundant, but keep it here for safety...
  if (length(along) > 1L || along < 1L || along > (N + 1L)) {
    txt <- paste("\"along\" must specify one dimension of the array,",
                 "or interpolate between two dimensions of the array",
                 sep = "\n")
    stop(simpleError(txt, call = abortcall))
  }
  if (along > N || along < 1L) {
    stop(simpleError(paste0("`along` must be between 0 and ", N), call = abortcall))
  }
  
  
  
  ## Dimension storing:
  arg.dim <- matrix(integer(1L), nrow = N, ncol = length(arg.list))
  if(name_along && !extra_dimensional) {
    arg.dimnames <- .rcpp_abind_get_dimnames(arg.list, along)
    arg.marginlen <- vapply(arg.list, \(x)dim(x)[along], integer(1L))
  }
  
  
  # START CORE FUNCTION
  pre <- seq_len(along - 1L)
  post <- seq(to = N - 1L, len = N - along)
  ## "perm" specifies permutation to put join dimension (along) last
  perm <- c(seq_len(N)[-along], along)
  
  for (i in seq_along(arg.list)) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    
    new.dim <- dim(m)
    if (length(new.dim) == N) {
      arg.dim[,i] <- new.dim
    }
    else if (length(new.dim) == (N - 1L)) {
      ## add another dimension
      arg.dim[,i] <- c(new.dim[pre], 1L, new.dim[post])
      dimnames(m) <- NULL # to prevent errors
      if (any(perm != seq_along(perm))) {
        dim(m) <- c(new.dim[pre], 1L, new.dim[post])
        m.changed <- TRUE
      }
    }
    else {
      txt <- paste0(
        "arg.list[[", i, "]]  does not fit: should have `length(dim())'=",
        N, " or ", N - 1L
      )
      stop(simpleError(txt, call = abortcall))
    }
    
    if (m.changed) {
      arg.list[[i]] <- m
    }
    else if (any(perm != seq_along(perm))) {
      arg.list[[i]] <- aperm.default(m, perm)
    }
  }
  # END CORE FUNCTION
  
  
  ## Make sure all dimensions conform
  conform.dim <- arg.dim[, 1L]
  if(!.rcpp_abind_all_conform_dims(conform.dim, arg.dim, ncol(arg.dim), along)) {
    stop(simpleError("non-conformable dimensions", call = abortcall))
  }
  
  # create output array (unordered)
  if(atomic) {
    out <- array(
      unlist(arg.list, use.names = FALSE, recursive = FALSE),
      dim = c(arg.dim[-along, 1L], sum(arg.dim[along, ]))
    )
  } else {
    out <- array(
      do.call(c, c(arg.list, use.names = FALSE)),
      dim = c(arg.dim[-along, 1L], sum(arg.dim[along, ]))
    )
  }
  
  
  ## permute the output array to put the join dimension back in the right place
  if (any(order(perm) != seq_along(perm))) {
    out <- aperm.default(out, order(perm))
  }
  
  # name_along:
  if(name_along) {
    if(!extra_dimensional) {
      .bind_set_dimnames(out, along, arg.list, arg.dimnames, arg.marginlen)
    }
    if(extra_dimensional) {
      if(!is.null(names(arg.list))) {
        dimnames(out)[[along]] <- names(arg.list)
      } else {
        dimnames(out)[[along]] <- stringi::stri_c("X", seq_len(dim(out)[along]))
      }
    }
  }
  
  return(out)
}


#' @keywords internal
#' @noRd
.bind_checkargs <- function(
    along, name_along, comnames_from, name_flat, abortcall
) {
  
  if(!is.numeric(along) || length(along) != 1) {
    stop(simpleError("`along` must be an integer scalar", call = abortcall))
  }
  
  if(!is.logical(name_along) || length(name_along) != 1) {
    stop(simpleError("`name_along` must be a Boolean", call = abortcall))
  }
  
  if(!is.null(comnames_from)) {
    if(!is.numeric(comnames_from) || length(comnames_from) != 1) {
      stop(simpleError("`comnames_from` must be an integer scalar or `NULL`", call = abortcall))
    }
  }
  
  if(!is.logical(name_flat) || length(name_flat) != 1) {
    stop(simpleError("`name_flat` must be a Boolean", call = abortcall))
  }
  
}

#' @keywords internal
#' @noRd
.bind_set_dimnames <- function(
    out, along, arg.list, arg.dimnames, arg.marginlen
) {
  name_along <- vector(mode = "character", length = dim(out)[along])
  arg.names <- names(arg.list)
  start.pos <- 0L
  for(i in seq_along(arg.list)) {
    marginlen <- arg.marginlen[i]
    indx <- seq_len(marginlen) + start.pos
    temp.dimnames <- .bind_getnames(arg.dimnames[[i]], arg.names[i], marginlen)
    collapse::setv(
      name_along, indx, temp.dimnames, vind1 = TRUE, xlist = FALSE
    )
    start.pos <- start.pos + marginlen
  }
  dimnames <- rep(list(NULL), length(dim(out)))
  dimnames[[along]] <- name_along
  data.table::setattr(out, "dimnames", dimnames)
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.bind_make_flatnames <- function(
    arg.list, along
) {
  out <- vector("list", length(arg.list))
  for(i in seq_along(arg.list)) {
    arg.current <- arg.list[[i]]
    temp <- .bind_getnames(names(arg.current), names(arg.list)[i], length(arg.current))
    dim(temp) <- dim(arg.current)
    out[[i]] <- temp
  }
  out <- .internal_abind(out, along, TRUE, FALSE)
  return(as.character(out))
}

#' @keywords internal
#' @noRd
.bind_set_sharednames <- function(out, sel, arg.list, along) {
  
  if(along == 0) {
    obj <- arg.list[[sel]]
    obj.dimnames <- dimnames(obj)
    out.dimnames <- dimnames(out)
    if(is.null(out.dimnames)) {
      out.dimnames <- rep(list(NULL), length(dim(out)))
    }
    if(!is.null(obj.dimnames)) {
      data.table::setattr(out, "dimnames", c(out.dimnames[1], obj.dimnames))
    }
    return(invisible(NULL))
  }
  
  N <- max(
    1L,
    vapply(arg.list, function(x) length(dim(x)), integer(1L))
  )
  if(along > N) {
    obj <- arg.list[[sel]]
    obj.dimnames <- dimnames(obj)
    out.dimnames <- dimnames(out)
    N <- length(out.dimnames)
    if(!is.null(obj.dimnames)) {
      data.table::setattr(out, "dimnames", c(obj.dimnames, out.dimnames[N]))
    }
    return(invisible(NULL))
  }
  
  
  obj <- arg.list[[sel]]
  obj.dimnames <- dimnames(obj)
  out.dimnames <- dimnames(out)
  if(is.null(out.dimnames)) {
    out.dimnames <- rep(list(NULL), length(dim(out)))
  }
  if(!is.null(obj.dimnames)) {
    out.dimnames[-along] <- obj.dimnames[-along]
    data.table::setattr(out, "dimnames", out.dimnames)
  }
  return(invisible(NULL))
}


#' @keywords internal
#' @noRd
.bind_getnames <- function(main.names, arg.name, size) {
  if(!is.null(main.names)) {
    temp.names <- main.names
  }
  else if(!is.null(arg.name)) {
    temp.names <- stringi::stri_c(arg.name, ".", seq_len(size))
  }
  else {
    temp.names <- stringi::stri_c("X", seq_len(size))
  }
  return(temp.names)
}

