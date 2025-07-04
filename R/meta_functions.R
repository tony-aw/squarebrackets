
#' @keywords internal
#' @noRd
.mybadge_class <- function(x) {
  txt <- paste0("class: ", x)
  file <- paste0("class-", gsub(" ", "_", x), "-blue.svg")
  text <- sprintf("\\link[=squarebrackets_indx_args]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

#' @keywords internal
#' @noRd
.mybadge_all_classes <- function() {
  txt <- "all classes"
  file <- "all_classes-blue.svg"
  text <- sprintf("\\link[=squarebrackets_indx_args]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_coercion <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("coercion_through_copy: ", x)
  file <- paste0("coercion_through_copy-", x2, ".svg")
  text <- sprintf("\\link[=squarebrackets_coercion]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

#' @keywords internal
#' @noRd
.mybadge_coercion_by_ref <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("coercion_by_reference: ", x)
  file <- paste0("coercion_by_reference-", x2, ".svg")
  text <- sprintf("\\link[=squarebrackets_coercion]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_coercion_through_copy <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("coercion_through_copy: ", x)
  file <- paste0("coercion_through_copy-", x2, ".svg")
  text <- sprintf("\\link[=squarebrackets_coercion]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_require_unique_names <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  txt <- paste0("require unique names: ", x)
  file <- paste0("require_unique_names-", x2, ".svg")
  text <- sprintf("\\link[=squarebrackets_technical]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_intro_section <- function(txt, colour) {
  txt <- toupper(txt)
  file <- paste0(gsub(" ", "_", tolower(txt)), "-", colour, ".svg")
  text <- sprintf("\\link[=squarebrackets_help]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_performance_set2 <- function(x) {
  if(x == "TRUE") x2 <- "TRUE-darkgreen"
  if(x == "FALSE") x2 <- "FALSE-red"
  txt <- paste0("for performance: set to ", x)
  file <- paste0("for_performance-set_to_", x2, ".svg")
  text <- sprintf("\\link[=squarebrackets_help]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

#' @keywords internal
#' @noRd
.mybadge_option <- function(type, x) {
  extra.txt <- ""
  extra.file <- ""
  if(type == "option") {
    extra.txt <- "squarebrackets."
    extra.file <- "squarebrackets_dot_"
  }
  txt <- paste0(type, ": ", extra.txt, x)
  file <- paste0(type, "-", extra.file, x, "-blue.svg")
  text <- sprintf("\\link[=squarebrackets_options]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}



# the following is the battle-tested abind::abind function,
# but adjusted so that it can also handle recursive arrays

#' @keywords internal
#' @noRd
.abind <- function(..., along=N, rev.along=NULL, new.names=NULL,
                       force.array=TRUE, make.names=use.anon.names, use.anon.names=FALSE,
                       use.first.dimnames=FALSE, hier.names=FALSE, use.dnns=FALSE)
{
  if (is.character(hier.names))
    hier.names <- match.arg(hier.names, c('before', 'after', 'none'))
  else
    hier.names <- if (hier.names) 'before' else 'no'
  arg.list <- list(...)
  if (is.list(arg.list[[1]]) && !is.data.frame(arg.list[[1]])) {
    if (length(arg.list)!=1)
      stop("can only supply one list-valued argument for ...")
    if (make.names)
      stop("cannot have make.names=TRUE with a list argument")
    arg.list <- arg.list[[1]]
    have.list.arg <- TRUE
  } else {
    N <- max(1, sapply(list(...), function(x) length(dim(x))))
    have.list.arg <- FALSE
  }
  if (any(discard <- sapply(arg.list, is.null)))
    arg.list <- arg.list[!discard]
  if (length(arg.list)==0)
    return(NULL)
  N <- max(1, sapply(arg.list, function(x) length(dim(x))))
  
  ## N will eventually be length(dim(return.value))
  if (!is.null(rev.along))
    along <- N + 1 - rev.along
  
  if (along < 1 || along > N || (along > floor(along) && along < ceiling(along))) {
    N <- N + 1
    along <- max(1, min(N+1, ceiling(along)))
  }
  
  ## this next check should be redundant, but keep it here for safety...
  if (length(along) > 1 || along < 1 || along > N + 1)
    stop(paste("\"along\" must specify one dimension of the array,",
               "or interpolate between two dimensions of the array",
               sep="\n"))
  
  if (!force.array && N==2) {
    if (!have.list.arg) {
      if (along==2)
        return(cbind(...))
      if (along==1)
        return(rbind(...))
    } else {
      if (along==2)
        return(do.call("cbind", arg.list))
      if (along==1)
        return(do.call("rbind", arg.list))
    }
  }
  
  if (along>N || along<0)
    stop("along must be between 0 and ", N)
  
  pre <- seq(from=1, length.out=along-1)
  post <- seq(to=N-1, length.out=N-along)
  ## "perm" specifies permutation to put join dimension (along) last
  perm <- c(seq(length.out=N)[-along], along)
  
  arg.names <- names(arg.list)
  if (is.null(arg.names)) arg.names <- rep("", length(arg.list))
  ## if new.names is a character vector, treat it as argument names
  if (is.character(new.names)) {
    arg.names[seq(along.with=new.names)[nchar(new.names)>0]] <-
      new.names[nchar(new.names)>0]
    new.names <- NULL
  }
  
  ## Be careful with dot.args, because if abind was called
  ## using do.call(), and had anonymous arguments, the expressions
  ## returned by match.call() are for the entire structure.
  ## This can be a problem in S-PLUS, not sure about R.
  ## E.g., in this one match.call() returns compact results:
  ## > (function(...)browser())(1:10,letters)
  ## Called from: (function(...)  browser())....
  ## b()> match.call(expand.dots=FALSE)$...
  ## list(1:10, letters)
  ## But in this one, match.call() returns evaluated results:
  ## > test <- function(...) browser()
  ## > do.call("test", list(1:3,letters[1:4]))
  ## Called from: test(c(1, 2, 3), c("a", "b....
  ## b(test)> match.call(expand.dots=FALSE)$...
  ## list(c(1, 2, 3), c("a", "b", "c", "d")
  ## The problem here was largely mitigated by making abind()
  ## accept a single list argument, which removes most of the
  ## need for the use of do.call("abind", ...)
  
  ## Create deparsed versions of actual arguments in arg.alt.names
  ## These are used for error messages
  if (any(arg.names=="")) {
    if (make.names) {
      ## Create dot.args to be a list of calling expressions for the objects to be bound.
      ## Be careful here with translation to R --
      ## dot.args does not have the "list" functor with R
      ## (and dot.args is not a call object), whereas with S-PLUS, dot.args
      ## must have the list functor removed
      dot.args <- match.call(expand.dots=FALSE)$... ## [[2]]
      if (is.call(dot.args) && identical(dot.args[[1]], as.name("list")))
        dot.args <- dot.args[-1]
      arg.alt.names <- arg.names
      for (i in seq(along.with=arg.names)) {
        if (arg.alt.names[i]=="") {
          if (utils::object.size(dot.args[[i]]) < 1000L) {
            arg.alt.names[i] <- paste(deparse(dot.args[[i]], 40), collapse=";")
          } else {
            arg.alt.names[i] <- paste("X", i, sep="")
          }
          arg.names[i] <- arg.alt.names[i]
        }
      }
      ## unset(dot.args) don't need dot.args any more, but R doesn't have unset()
    } else {
      arg.alt.names <- arg.names
      arg.alt.names[arg.names==""] <- paste("X", seq(along.with=arg.names), sep="")[arg.names==""]
    }
  } else {
    arg.alt.names <- arg.names
  }
  
  use.along.names <- any(arg.names!="")
  
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
  
  for (i in seq(length.out=length(arg.list))) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    
    ## be careful with conversion to array: as.array converts data frames badly
    if (is.data.frame(m)) {
      ## use as.matrix() in preference to data.matrix() because
      ## data.matrix() uses the unintuitive codes() function on factors
      m <- as.matrix(m)
      m.changed <- TRUE
    } else if (!is.array(m) && !is.null(m)) {
      if (!is.atomic(m))
        stop("arg '", arg.alt.names[i], "' is non-atomic")
      ## make sure to get the names of a vector and attach them to the array
      dn <- names(m)
      m <- as.array(m)
      if (length(dim(m))==1 && !is.null(dn))
        dimnames(m) <- list(dn)
      m.changed <- TRUE
    }
    new.dim <- dim(m)
    if (length(new.dim)==N) {
      ## Assign the dimnames of this argument to the i'th column of arg.dimnames.
      ## If dimnames(m) is NULL, would need to do arg.dimnames[,i] <- list(NULL)
      ## to set all elts to NULL, as arg.dimnames[,i] <- NULL does not actually
      ## change anything in S-PLUS (leaves whatever is there) and illegal in R.
      ## Since arg.dimnames has NULL entries to begin with, don't need to do
      ## anything when dimnames(m) is NULL
      if (!is.null(dimnames(m))) {
        arg.dimnames[,i] <- dimnames(m)
        if (use.dnns && !is.null(names(dimnames(m))))
          arg.dnns[,i] <- as.list(names(dimnames(m)))
      }
      arg.dim[,i] <- new.dim
    } else if (length(new.dim)==N-1) {
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
      arg.dim[,i] <- c(new.dim[pre], 1, new.dim[post])
      if (any(perm!=seq(along.with=perm))) {
        dim(m) <- c(new.dim[pre], 1, new.dim[post])
        m.changed <- TRUE
      }
    } else {
      stop("'", arg.alt.names[i], "' does not fit: should have `length(dim())'=",
           N, " or ", N-1)
    }
    
    if (any(perm!=seq(along.with=perm)))
      arg.list[[i]] <- aperm(m, perm)
    else if (m.changed)
      arg.list[[i]] <- m
  }
  
  ## Make sure all arguments conform
  conform.dim <- arg.dim[,1]
  for (i in seq(length.out=ncol(arg.dim))) {
    if (any((conform.dim!=arg.dim[,i])[-along])) {
      stop("arg '", arg.alt.names[i], "' has dims=", paste(arg.dim[,i], collapse=", "),
           "; but need dims=", paste(replace(conform.dim, along, "X"), collapse=", "))
    }
  }
  
  ## find the last (or first) names for each dimensions except the join dimension
  if (N>1)
    for (dd in seq(length.out=N)[-along]) {
      for (i in (if (use.first.dimnames) seq(along.with=arg.names) else rev(seq(along.with=arg.names)))) {
        if (length(arg.dimnames[[dd,i]]) > 0) {
          dimnames.new[[dd]] <- arg.dimnames[[dd,i]]
          if (use.dnns && !is.null(arg.dnns[[dd,i]]))
            names(dimnames.new)[dd] <- arg.dnns[[dd,i]]
          break
        }
      }
    }
  
  ## find or create names for the join dimension
  for (i in seq(length.out=length(arg.names))) {
    ## only use names if arg i contributes some elements
    if (arg.dim[along,i] > 0) {
      dnm.along <- arg.dimnames[[along,i]]
      if (length(dnm.along)==arg.dim[along,i]) {
        use.along.names <- TRUE
        if (hier.names=='before' && arg.names[i]!="")
          dnm.along <- paste(arg.names[i], dnm.along, sep=".")
        else if (hier.names=='after' && arg.names[i]!="")
          dnm.along <- paste(dnm.along, arg.names[i], sep=".")
      } else {
        ## make up names for the along dimension
        if (arg.dim[along,i]==1)
          dnm.along <- arg.names[i]
        else if (arg.names[i]=="")
          dnm.along <- rep("", arg.dim[along,i])
        else
          dnm.along <- paste(arg.names[i], seq(length.out=arg.dim[along,i]), sep="")
      }
      dimnames.new[[along]] <- c(dimnames.new[[along]], dnm.along)
    }
    if (use.dnns) {
      dnn <- unlist(arg.dnns[along,])
      if (length(dnn)) {
        if (!use.first.dimnames)
          dnn <- rev(dnn)
        names(dimnames.new)[along] <- dnn[1]
      }
    }
  }
  ## if no names at all were given for the along dimension, use none
  if (!use.along.names)
    dimnames.new[along] <- list(NULL)
  
  ## Construct the output array from the pieces.
  ## Could experiment here with more efficient ways of constructing the
  ## result than using unlist(), e.g.
  ##    out <- numeric(prod(c( arg.dim[-along,1], sum(arg.dim[along,]))))
  ## Don't use names in unlist because this can quickly exhaust memory when
  ## abind is called with "do.call" (which creates horrendous names in S-PLUS).
  out <- array(unlist(arg.list, use.names=FALSE, recursive = FALSE),
               dim=c( arg.dim[-along,1], sum(arg.dim[along,])),
               dimnames=dimnames.new[perm])
  ## permute the output array to put the join dimension back in the right place
  if (any(order(perm)!=seq(along.with=perm)))
    out <- aperm(out, order(perm))
  
  ## if new.names is list of character vectors, use whichever are non-null
  ## for dimension names, checking that they are the right length
  if (!is.null(new.names) && is.list(new.names)) {
    for (dd in seq(length.out=N)) {
      if (!is.null(new.names[[dd]])) {
        if (length(new.names[[dd]])==dim(out)[dd])
          dimnames(out)[[dd]] <- new.names[[dd]]
        else if (length(new.names[[dd]]))
          warning(paste("Component ", dd,
                        " of new.names ignored: has length ",
                        length(new.names[[dd]]), ", should be ",
                        dim(out)[dd], sep=""))
      }
      if (use.dnns && !is.null(names(new.names)) && names(new.names)[dd]!='')
        names(dimnames(out))[dd] <- names(new.names)[dd]
    }
  }
  if (use.dnns && !is.null(names(dimnames(out))) && any(i <- is.na(names(dimnames(out)))))
    names(dimnames(out))[i] <- ''
  out
}



#' @keywords internal
#' @noRd
.internal_get_protected_addresses_base <- function() {
  env <- baseenv()
  nms <- setdiff(
    ls(env, all.names = TRUE),
    invisible(utils::lsf.str(envir = env, all.names = TRUE))
  )
  protected_binds <- vapply(
    nms,
    \(x) bindingIsLocked(x, env = env) || bindingIsActive(x, env = env),
    logical(1L)
  )
  nms <- setdiff(
    nms[protected_binds],
    c(".Last.value", "Last.value")
  )
  lst <- as.list(env, all.names = TRUE)[nms]
  lst <- rapply(lst, .rcpp_address)
  return(lst)
}

#' @keywords internal
#' @noRd
.internal_get_protected_addresses <- function(env) {
  nms <- setdiff(
    ls(env, all.names = TRUE),
    invisible(utils::lsf.str(envir = env, all.names = TRUE))
  )
  protected_binds <- vapply(
    nms,
    \(x) bindingIsLocked(x, env = env) || bindingIsActive(x, env = env),
    logical(1L)
  )
  nms <- setdiff(
    nms[protected_binds],
    c(".Last.value", "Last.value")
  )
  lst <- as.list(env, all.names = TRUE)[nms]
  subenvs <- vapply(
    lst, is.environment, logical(1L)
  )
  lst[subenvs] <- lapply(lst[subenvs], as.list)
  lst <- rapply(lst, .rcpp_address)
  return(lst)
}

#' @keywords internal
#' @noRd
.protected_addresses <- function() {
  lst1 <- .internal_get_protected_addresses_base()
  lst2 <- .internal_get_protected_addresses(loadNamespace("utils"))
  lst <- c(unlist(lst1), unlist(lst2))
  return(unlist(lst))
}

