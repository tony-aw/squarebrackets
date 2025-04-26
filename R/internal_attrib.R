

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

