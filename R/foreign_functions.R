

#' @keywords internal
#' @noRd
.abind.recursive <- function(
    arg.list, along = N, rev.along = NULL, new.names = NULL,
    use.first.dimnames = FALSE, hier.names = FALSE, use.dnns = FALSE)
{
  # This function is a modified version of abind::abind.
  # The modification is different in 4 ways:
  # 1) .abind.recursive can bind recursive arrays (i.e. dimensional lists), whereas abind::abind can't.
  # 2) .abind.recursive is faster, through more efficient code and packages 'collapse' and 'Rcpp'.
  # 3) The main input is a list, instead of an ellipsis.
  # 4) some arguments have been disabled or set at a fixed value for simplicity.
  
  # make.names <- use.anon.names <- FALSE # cannot have make.names=TRUE with a list argument
  # force.array <- TRUE # needs to be TRUE, otherwise results will be inconsistent for dim = 1 or 2.
  
  if (is.character(hier.names)) {
    hier.names <- match.arg(hier.names, c('before', 'after', 'none'))
  }
  else {
    hier.names <- if (hier.names) 'before' else 'no'
  }
  
  # have.list.arg <- TRUE
  
  N <- max(
    1,
    vapply(arg.list, function(x) length(dim(x)), integer(1))
  )
  
  ## N will eventually be length(dim(return.value))
  if (!is.null(rev.along)) {
    along <- N + 1L - rev.along
  }
  
  if (along < 1L || along > N || (along > floor(along) && along < ceiling(along))) {
    N <- N + 1L
    along <- max(1L, min(N + 1L, ceiling(along)))
  }
  
  ## this next check should be redundant, but keep it here for safety...
  if (length(along) > 1L || along < 1L || along > (N + 1L))
    stop(paste("\"along\" must specify one dimension of the array,",
               "or interpolate between two dimensions of the array",
               sep = "\n"))
  
  
  if (along > N || along < 1L)
    stop("along must be between 1 and ", N)
  
  pre <- seq_len(along - 1L)
  post <- seq(to = N - 1L, len = N - along)
  ## "perm" specifies permutation to put join dimension (along) last
  perm <- c(seq_len(N)[-along], along)
  
  arg.names <- names(arg.list)
  if (is.null(arg.names)) {
    arg.names <- collapse::alloc("", length(arg.list))
  }
  ## if new.names is a character vector, treat it as argument names
  if (is.character(new.names)) {
    arg.names[seq_along(new.names)[nchar(new.names)>0]] <-
      new.names[nchar(new.names)>0]
    new.names <- NULL
  }
  
  ## Create deparsed versions of actual arguments in arg.alt.names
  ## These are used for error messages
  if (collapse::anyv(arg.names, "")) {
    arg.alt.names <- arg.names
    temp.indx <- collapse::whichv(arg.names, "")
    arg.alt.names[temp.indx] <- paste("X", seq_along(arg.names), sep="")[temp.indx]
  }
  else {
    arg.alt.names <- arg.names
  }
  
  use.along.names <- !collapse::allv(arg.names, "")
  
  ## need to have here: arg.names, arg.alt.names, don't need dot.args
  
  names(arg.list) <- arg.names
  ## arg.dimnames is a matrix of dimension names, each element of the
  ## the matrix is a character vector, e.g., arg.dimnames[j,i] is
  ## the vector of names for dimension j of arg i
  arg.dimnames <- matrix(vector("list", N * length(arg.names)), nrow = N, ncol = length(arg.names))
  dimnames(arg.dimnames) <- list(NULL, arg.names)
  ## arg.dnns is a matrix of names of dimensions, each element is a
  ## character vector len 1, or NULL
  arg.dnns <- matrix(vector("list", N * length(arg.names)), nrow = N, ncol = length(arg.names))
  dimnames(arg.dnns) <- list(NULL, arg.names)
  dimnames.new <- vector("list", N)
  
  ## Coerce all arguments to have the same number of dimensions
  ## (by adding one, if necessary) and permute them to put the
  ## join dimension last.
  
  ## Create arg.dim as a matrix with length(dim) rows and
  ## length(arg.list) columns: arg.dim[j,i]==dim(arg.list[[i]])[j],
  ## The dimension order of arg.dim is original
  arg.dim <- matrix(integer(1L), nrow = N, ncol = length(arg.names))
  
  for (i in seq_along(arg.list)) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    
    new.dim <- dim(m)
    if (length(new.dim) == N) {
      if (!is.null(dimnames(m))) {
        arg.dimnames[,i] <- dimnames(m)
        if (use.dnns && !is.null(names(dimnames(m)))) {
          arg.dnns[,i] <- as.list(names(dimnames(m)))
        }
        
      }
      arg.dim[,i] <- new.dim
    }
    else if (length(new.dim) == (N - 1L)) {
      ## add another dimension (first set dimnames to NULL to prevent errors)
      if (!is.null(dimnames(m))) {
        ## arg.dimnames[,i] <- c(dimnames(m)[pre], list(NULL), dimnames(m))[post]
        ## is equivalent to arg.dimnames[-N,i] <- dimnames(m)
        arg.dimnames[-along,i] <- dimnames(m)
        if (use.dnns && !is.null(names(dimnames(m))))
          arg.dnns[-along,i] <- as.list(names(dimnames(m)))
        ## remove the dimnames so that we can assign a dim of an extra length
        dimnames(m) <- NULL
      }
      arg.dim[,i] <- c(new.dim[pre], 1L, new.dim[post])
      if (any(perm != seq_along(perm))) {
        dim(m) <- c(new.dim[pre], 1, new.dim[post])
        m.changed <- TRUE
      }
    }
    else {
      stop("'", arg.alt.names[i], "' does not fit: should have `length(dim())'=",
           N, " or ", N - 1L)
    }
    
    if (any(perm != seq_along(perm))) {
      arg.list[[i]] <- aperm(m, perm)
    }
    else if (m.changed) {
      arg.list[[i]] <- m
    }
  }
  
  ## Make sure all arguments conform (re-written this part using 'Rcpp')
  conform.dim <- arg.dim[,1]
  .rcpp_check_conform_dims(conform.dim, arg.dim, ncol(arg.dim), along)
  
  ## find the last (or first) names for each dimensions except the join dimension
  if (N > 1L)
    for (dd in seq_len(N)[-along]) {
      if (use.first.dimnames) {
        loopseq <- seq_along(arg.names)
      }
      else {
        loopseq <- rev(seq_along(arg.names))
      }
      
      for (i in loopseq) {
        if (length(arg.dimnames[[dd,i]]) > 0L) {
          dimnames.new[[dd]] <- arg.dimnames[[dd,i]]
          if (use.dnns && !is.null(arg.dnns[[dd,i]])) {
            names(dimnames.new)[dd] <- arg.dnns[[dd,i]]
          }
          
          break
        }
      }
    }
  
  ## find or create names for the join dimension
  for (i in seq_along(arg.names)) {
    ## only use names if arg i contributes some elements
    if (arg.dim[along,i] > 0) {
      dnm.along <- arg.dimnames[[along,i]]
      if (length(dnm.along) == arg.dim[along,i]) {
        use.along.names <- TRUE
        if (hier.names == 'before' && arg.names[i]!="") {
          dnm.along <- paste(arg.names[i], dnm.along, sep=".")
        }
        else if (hier.names == 'after' && arg.names[i] != "") {
          dnm.along <- paste(dnm.along, arg.names[i], sep=".")
        }
      }
      else {
        ## make up names for the along dimension
        if (arg.dim[along,i] == 1L) {
          dnm.along <- arg.names[i]
        }
        else if (arg.names[i]=="") {
          dnm.along <- collapse::alloc("", arg.dim[along,i])
        }
        else {
          dnm.along <- paste(arg.names[i], seq_len(arg.dim[along,i]), sep="")
        }
      }
      dimnames.new[[along]] <- c(dimnames.new[[along]], dnm.along)
    }
    if (use.dnns) {
      dnn <- unlist(arg.dnns[along,])
      if (length(dnn)) {
        if (!use.first.dimnames) {
          dnn <- rev(dnn)
        }
        names(dimnames.new)[along] <- dnn[1]
      }
    }
  }
  ## if no names at all were given for the along dimension, use none
  if (!use.along.names) {
    dimnames.new[along] <- list(NULL)
  }
  
  out <- do.call(c, arg.list)
  out <- array(
    out, dim = c(arg.dim[-along, 1L], sum(arg.dim[along,])),
    dimnames = dimnames.new[perm]
  )
  
  ## permute the output array to put the join dimension back in the right place
  if (any(order(perm) != seq_along(perm))) {
    out <- aperm(out, order(perm))
  }
  
  ## if new.names is list of character vectors, use whichever are non-null
  ## for dimension names, checking that they are the right length
  if (!is.null(new.names) && is.list(new.names)) {
    for (dd in seq_len(N)) {
      if (!is.null(new.names[[dd]])) {
        if (length(new.names[[dd]]) == dim(out)[dd]) {
          dimnames(out)[[dd]] <- new.names[[dd]]
        }
        else if (length(new.names[[dd]])) {
          warning(paste("Component ", dd,
                        " of new.names ignored: has length ",
                        length(new.names[[dd]]), ", should be ",
                        dim(out)[dd], sep=""))
        }
      }
      if (use.dnns && !is.null(names(new.names)) && names(new.names)[dd]!='')
        names(dimnames(out))[dd] <- names(new.names)[dd]
    }
  }
  if (use.dnns && !is.null(names(dimnames(out))) && any(i <- is.na(names(dimnames(out)))))
    names(dimnames(out))[i] <- ''
  return(out)
}
