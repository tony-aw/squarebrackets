
#' @keywords internal
#' @noRd
.abind.recursive <- function(arg.list, along=N) {
  # adapted from the 'abind' package
  # rev.along <- NULL
  # new.names <- NULL
  # force.array <- TRUE
  # make.names <- use.anon.names <- FALSE
  use.first.dimnames <- FALSE
  #  hier.names = FALSE
  use.dnns <- FALSE
  hier.names <- 'no'
  # have.list.arg <- TRUE
  N <- max(
    1,
    vapply(arg.list, function(x) length(dim(x)), integer(1))
  )
  
  if (along > N || along < 1L)
    stop("along must be between 1 and ", N)
  
  pre <- seq_len(along - 1L)
  post <- seq(to=N-1, len=N-along)
  ## "perm" specifies permutation to put join dimension (along) last
  perm <- c(seq_len(N)[-along], along)
  
  arg.names <- collapse::alloc("", length(arg.list))
  
  ## Create deparsed versions of actual arguments in arg.alt.names
  ## These are used for error messages
  arg.alt.names <- arg.names
  arg.alt.names[arg.names==""] <- paste("X", seq_along(arg.names), sep="")[arg.names==""]
  use.along.names <- FALSE
  
  ## need to have here: arg.names, arg.alt.names, don't need dot.args
  
  names(arg.list) <- arg.names
  ## arg.dimnames is a matrix of dimension names, each element of the
  ## the matrix is a character vector, e.g., arg.dimnames[j,i] is
  ## the vector of names for dimension j of arg i
  arg.dimnames <- matrix(vector("list", N*length(arg.names)), nrow=N, ncol=length(arg.names))
  dimnames(arg.dimnames) <- list(NULL, arg.names)
  ## arg.dnns is a matrix of names of dimensions, each element is a
  ## character vector len 1, or NULL
  arg.dnns <- matrix(vector("list", N*length(arg.names)), nrow=N, ncol=length(arg.names))
  dimnames(arg.dnns) <- list(NULL, arg.names)
  dimnames.new <- vector("list", N)
  
  ## Coerce all arguments to have the same number of dimensions
  ## (by adding one, if necessary) and permute them to put the
  ## join dimension last.
  
  ## Create arg.dim as a matrix with length(dim) rows and
  ## length(arg.list) columns: arg.dim[j,i]==dim(arg.list[[i]])[j],
  ## The dimension order of arg.dim is original
  arg.dim <- matrix(integer(1), nrow=N, ncol=length(arg.names))
  
  for (i in seq_along(arg.list)) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    new.dim <- dim(m)
    if (length(new.dim) == N) {
      arg.dim[,i] <- new.dim
    }
    else if (length(new.dim) == (N-1)) {
      ## add another dimension (first set dimnames to NULL to prevent errors)
      arg.dim[,i] <- c(new.dim[pre], 1, new.dim[post])
      if (any(perm != seq_along(perm))) {
        dim(m) <- c(new.dim[pre], 1, new.dim[post])
        m.changed <- TRUE
      }
    }
    else {
      stop("'", arg.alt.names[i], "' does not fit: should have `length(dim())'=",
           N, " or ", N-1)
    }
    
    if (any(perm != seq_along(perm))) {
      arg.list[[i]] <- aperm(m, perm)
    }
    else if (m.changed) {
      arg.list[[i]] <- m
    }
  }
  
  ## Make sure all arguments conform
  conform.dim <- arg.dim[,1]
  for (i in collapse::seq_col(arg.dim)) {
    if (any((conform.dim != arg.dim[,i])[-along])) {
      stop("arg '", arg.alt.names[i], "' has dims=", paste(arg.dim[,i], collapse=", "),
           "; but need dims=", paste(replace(conform.dim, along, "X"), collapse=", "))
    }
  }
  
  ## find the last (or first) names for each dimensions except the join dimension
  if (N>1) {
    for (dd in seq_len(N)[-along]) {
      if (use.first.dimnames) {
        loopseq <- seq_along(arg.names)
      } else {
        loopseq <- rev(seq_along(arg.names))
      }
      
      for (i in loopseq) {
        if (length(arg.dimnames[[dd,i]]) > 0) {
          dimnames.new[[dd]] <- arg.dimnames[[dd,i]]
          if (use.dnns && !is.null(arg.dnns[[dd,i]])) {
            names(dimnames.new)[dd] <- arg.dnns[[dd,i]]
          }
          break
        }
      }
    }
  }
  
  
  ## find or create names for the join dimension
  for (i in seq_along(arg.names)) {
    ## only use names if arg i contributes some elements
    if (arg.dim[along,i] > 0) {
      dnm.along <- arg.dimnames[[along,i]]
      if (length(dnm.along)==arg.dim[along,i]) {
        use.along.names <- TRUE
        if (hier.names=='before' && arg.names[i]!="") {
          dnm.along <- paste(arg.names[i], dnm.along, sep=".")
        }
        else if (hier.names=='after' && arg.names[i]!="")
          dnm.along <- paste(dnm.along, arg.names[i], sep=".")
      } else {
        ## make up names for the along dimension
        if (arg.dim[along,i]==1) {
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
  if (!use.along.names){
    dimnames.new[along] <- list(NULL)
  }
  
  ## Construct the output recursive array from the pieces.
  out <- do.call(c, arg.list)
  out <- array(
    out, dim = c( arg.dim[-along,1], sum(arg.dim[along,])),
    dimnames = dimnames.new[perm]
  )
  ## permute the output array to put the join dimension back in the right place
  if (any(order(perm) != seq_along(perm))) {
    out <- aperm(out, order(perm))
  }
  if (use.dnns && !is.null(names(dimnames(out))) && any(i <- is.na(names(dimnames(out))))) {
    names(dimnames(out))[i] <- ''
  }
  
  return(out)
}
