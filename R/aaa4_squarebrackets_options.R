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
#' @section Retain Attributes:
#' `r .mybadge_option("argument", "rat")` \cr
#' `r .mybadge_option("option", "rat")` \cr
#' Like the `[` - methods,
#' \link{sb_x} and \link{sb_rm} will by default
#' strip most (but not all) attributes. \cr
#' The \link{sb_x} and \link{sb_rm} methods
#' in 'squarebrackets' have the `rat` argument,
#' to control this control the attribute stripping behaviour. \cr
#' If `rat = FALSE`, this default behaviour is preserved,
#' for compatibility with special classes. This is the fastest option. \cr
#' If `rat = TRUE`,
#' attributes from `x` missing after sub-setting are re-assigned to `x`.
#' Already existing attributes after sub-setting will not be overwritten. \cr
#' There is no `rat` argument for data.frame-like object:
#' their attributes will always be preserved. \cr
#' NOTE: In the following situations, the `rat` argument will be ignored,
#' as the attributes necessarily have to be dropped:
#'  * when `x` is a list, AND `drop = TRUE`,
#'  AND a single element is selected,
#'  AND sub-setting is done through the `i` argument.
#'  * when `x` is an atomic matrix or array,
#'  and sub-setting is done through the `i` argument. \cr
#'  
#' The default value for the `rat` argument can be changed in the `squarebrackets.rat` option. \cr \cr
#' 
#' 
#' @section Mutable Atomic Messages:
#' `r .mybadge_option("option", "ma_messages")` \cr
#' The `[<-.mutable_atomic` method notifies the user of copy-on-modification. \cr
#' Should the user find this annoying,
#' the user can disable these messages
#' by setting `squarebrackets.ma_messages` to `FALSE`. \cr \cr
#' 
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
