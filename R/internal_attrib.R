

#' @keywords internal
#' @noRd
.internal_ma_set_DimsAndNames <- function(x, names = NULL, dims = NULL, dimnames = NULL) {
  
  # set dims, names, and dimnames of a mutable_atomic object BY REFERENCE
  
  if(!is.mutable_atomic(x)) {
    stop("not mutable_atomic")
  }
  
  
  if(!is.null(dims)) {
    data.table::setattr(x, "dim", data.table::copy(dims))
  }
  if(!is.null(dimnames)) {
    dimnames <- data.table::copy(dimnames) # protection against pass-by-reference
    data.table::setattr(x, "dimnames", NULL)
    data.table::setattr(x, "dimnames", dimnames)
  }
  if(!is.null(names)) {
    names <- data.table::copy(names) # protection against pass-by-reference
    data.table::setattr(x, "names", NULL)
    data.table::setattr(x, "names", names)
  }
}

#' @keywords internal
#' @noRd
.internal_specialattrib <- function() {
  out <- c(
    "comment", "dim", "dimnames", "names", "row.names", "col.names", "tsp",
    "serial"
  )
  return(out)
}


#' @keywords internal
#' @noRd
.internal_set_stickyattr <- function(output, input) {
  input.attr <- names(attributes(input))
  attr.ignore <- unique(c(.internal_specialattrib(), names(attributes(output))))
  attr.add <- setdiff(
    input.attr, attr.ignore
  )
  if(length(attr.add) > 0L) {
    for(i in attr.add) {
      tryCatch(
        {data.table::setattr(output, i, attr(input, i, exact = TRUE))},
        error = function(){
          txt <- simpleMessage(paste0("cannot add attribute: ", i))
          message(txt)
        }
      )
    }
  }
}
