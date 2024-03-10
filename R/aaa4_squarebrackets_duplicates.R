#' On Duplicates
#'
#' 
#' @description
#' 
#' The \link{sb_x} method is the only method where providing duplicate indices actually make sense. \cr
#' For the other methods, it doesn't make sense. \cr
#' Giving duplicate indices usually won't break anything;
#' however, when replacing/transforming or removing subsets,
#' it is almost certainly not the intention to provide duplicate indices. \cr
#' Providing duplicate indices anyway might lead to unexpected results. \cr
#' Therefore, for the methods where giving duplicate indices does not make sense,
#' the `chkdup` argument is present. \cr
#' This argument controls whether the method in question checks for duplicates (`TRUE`) or not (`FALSE`). \cr
#' \cr
#' By default, `chkdup = TRUE`, which means the method in question will check for duplicate indices,
#' and give an error when it finds them. \cr
#' \cr
#' Setting `chkdup = FALSE` will disable these checks,
#' which saves time and computation power, and is thus more efficient. \cr
#' \cr
#' Checking for duplicates, is not too expensive,
#' so for the sake of user-friendliness, the default is `chkdup = TRUE`. \cr
#' For optimal performance, please set `chkdup = FALSE`.
#' 
#' 
#' @rdname aaa4_squarebrackets_duplicates
#' @name aaa4_squarebrackets_duplicates
#' @aliases squarebrackets_duplicates
NULL
