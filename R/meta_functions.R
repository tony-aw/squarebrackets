
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


#' @keywords internal
#' @noRd
.old_approx_empty_df <- function(x, row, col, class) {
  if(class == "data.frame") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qDF(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
  if(class == "data.table") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qDT(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
  if(class == "tibble") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qTBL(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
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


#' @keywords internal
#' @noRd
.old_coord2ind <- function(coord, x.dim, checks = TRUE) {
  n <- length(x.dim)
  
  if(checks) {
    if(n == 0L) {
      stop("`length(x.dim) == 0`")
    }
    
    if(!is.numeric(x.dim) || !is.numeric(coord)) {
      stop("`x.dim` and `coord` must both be numeric")
    }
    
    if(!isTRUE(collapse::fncol(coord) == n)) {
      stop("`ncol(coord) != length(x.dim)`")
    }
  }
  
  ind2 <- coord[, 1L, drop = TRUE]
  
  if(n > 1L) {
    for(i in seq.int(n, 2L)) {
      myprod <- prod(x.dim[seq_len(i - 1L)])
      ind2 <- as.integer(
        ind2 + myprod * (coord[, i, drop = TRUE] - 1L)
      )
    }
  }
  
  return(ind2)
}
