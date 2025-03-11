# set-up ====

library(stringi)

dimname <- c("row", "col", "rowcol")
args <- c("row", "col", "row, col")
asint <- c(
  "row <- as.integer(row - 1L)",
  "col <- as.integer(col - 1L)",
  paste("row <- as.integer(row - 1L)", "\n", "col <- as.integer(col - 1L)")
)

Rtemplate <- "

#' @keywords internal
#' @noRd
.rcpp_set_DIMNAME <- function(x, ARGS, rp, abortcall) {
  ASINT
  if(is.logical(x)) {
    .rcpp_set_DIMNAME_Logical(x, ARGS, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_DIMNAME_Integer(x, ARGS, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_DIMNAME_Numeric(x, ARGS, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_DIMNAME_Character(x, ARGS, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_DIMNAME_Complex(x, ARGS, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_DIMNAME_Raw(x, ARGS, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      \"unsupported matrix type\", call = abortcall
    ))
  }

}

"

rtemplate_codes <- list()

for(i in seq_along(dimname)) {
  rtemplate_codes[[i]] <- stri_replace_all(
    Rtemplate,
    fixed = c("DIMNAME", "ARGS", "ASINT"),
    replacement = c(dimname[i], args[i], asint[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}

rtemplate_codes <- do.call(paste, rtemplate_codes)
cat(rtemplate_codes)

Rtemplate1 <- "

#' @keywords internal
#' @noRd
.rcpp_set_DIMNAME1 <- function(x, ARGS, rp, abortcall) {
  ASINT
  if(is.logical(x)) {
    .rcpp_set_DIMNAME_Logical1(x, ARGS, as.logical(rp))
    return(invisible(NULL))
  }
  else if(is.integer(x)) {
    .rcpp_set_DIMNAME_Integer1(x, ARGS, as.integer(rp))
    return(invisible(NULL))
  }
  else if(is.double(x)) {
    .rcpp_set_DIMNAME_Numeric1(x, ARGS, as.double(rp))
    return(invisible(NULL))
  }
  else if(is.character(x)) {
    .rcpp_set_DIMNAME_Character1(x, ARGS, as.character(rp))
    return(invisible(NULL))
  }
  else if(is.complex(x)) {
    .rcpp_set_DIMNAME_Complex1(x, ARGS, as.complex(rp))
    return(invisible(NULL))
  }
  else if(is.raw(x)) {
    .rcpp_set_DIMNAME_Raw1(x, ARGS, as.raw(rp))
    return(invisible(NULL))
  }
  else {
    stop(simpleError(
      \"unsupported matrix type\", call = abortcall
    ))
  }

}


"


rtemplate_codes1 <- list()

for(i in seq_along(dimname)) {
  rtemplate_codes1[[i]] <- stri_replace_all(
    Rtemplate1,
    fixed = c("DIMNAME", "ARGS", "ASINT"),
    replacement = c(dimname[i], args[i], asint[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}

rtemplate_codes1 <- do.call(paste, rtemplate_codes1)
cat(rtemplate_codes1)


header <- "

#' @keywords internal
#' @noRd
.set_mat <- function(x, row, col, rp, tf, abortcall) {
  
  # CASE 1: rows specified, columns missing
  if(is.null(col)) { 
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        \"`tf` must be a function\", call = abortcall
      ))
      rp = tf(x[row, ])
    }
    if(length(rp) == 1) {
      .rcpp_set_row1(x, row, rp, abortcall)
      return(invisible(NULL))
    }
    .check_rp_atomic(rp, length(row) * ncol(x), abortcall)
    .rcpp_set_row(x, row, rp, abortcall)
    return(invisible(NULL))
  }
  
  # CASE 2: columns specified, rows missing
  if(is.null(row)) { 
    if(!missing(tf)) {
      if(!is.function(tf)) stop(simpleError(
        \"`tf` must be a function\", call = abortcall
      ))
      rp = tf(x[, col])
    }
    if(length(rp) == 1) {
      .rcpp_set_col1(x, col, rp, abortcall)
      return(invisible(NULL))
    }
    .check_rp_atomic(rp, length(col) * nrow(x), abortcall)
    .rcpp_set_col(x, col, rp, abortcall)
    return(invisible(NULL))
  }
  
  # CASE 3: rows AND columns specified:
  if(!missing(tf)) {
    if(!is.function(tf)) stop(simpleError(
      \"`tf` must be a function\", call = abortcall
    ))
    rp = tf(x[row, col])
  }
  if(length(rp) == 1) {
    .rcpp_set_rowcol1(x, row, col, rp, abortcall)
    return(invisible(NULL))
  }
  .check_rp_atomic(rp, length(row) * length(col), abortcall)
  .rcpp_set_rowcol(x, row, col, rp, abortcall)
  return(invisible(NULL))
  
}

"

rtemplate_all <- paste(header, "\n\n\n", rtemplate_codes, "\n\n\n", rtemplate_codes1)

fileConn <- file("R/dynamic_set_rowcol.R")
writeLines(rtemplate_all, fileConn)
close(fileConn)
