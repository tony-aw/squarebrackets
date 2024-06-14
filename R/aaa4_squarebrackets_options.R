#' squarebrackets Options
#'
#' 
#' @description
#' This help page explains the various global options that can be set for the 'squarebrackets' package,
#' and how it affects the functionality. \cr \cr
#' 
#' 
#' @section Check Duplicates:
#' `r .mybadge_option("argument", "chkdup")` \cr
#' `r .mybadge_option("option", "chkdup")` \cr
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
#' Setting `chkdup = TRUE` means the method in question will check for duplicate indices,
#' and give an error when it finds them. \cr
#' \cr
#' Setting `chkdup = FALSE` will disable these checks,
#' which saves time and computation power, and is thus more efficient. \cr
#' \cr
#' Since checking for duplicates can be expensive, it is set to `FALSE` by default. \cr
#' The default can be changed in the `squarebrackets.chkdup` option. \cr \cr
#' 
#' 
#' @section Mutable Atomic Messages:
#' `r .mybadge_option("option", "ma_messages")` \cr
#' The `[<-.mutable_atomic` method notifies the user of copy-on-modification. \cr
#' Should the user find this annoying,
#' the user can disable these messages
#' by setting `squarebrackets.ma_messages` to `FALSE`. \cr
#' \cr
#' 
#' @section squarebrackets.protected:
#' The user should NEVER touch the `squarebrackets.protected` option. \cr
#' This option lists all locked non-functions in the base environment,
#' in order to protect them from any accidental pass-by-reference modification
#' by the methods/functions from 'squarebrackets'. \cr
#' Other packages that provide pass-by-reference modification,
#' such as the 'collapse' package,
#' generally do not provide such protections,
#' and are not blocked by `squarebrackets.protected`. \cr \cr
#' 
#' 
#' @rdname aaa4_squarebrackets_options
#' @name aaa4_squarebrackets_options
#' @aliases squarebrackets_options
NULL
