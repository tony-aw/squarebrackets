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
#' The default can be changed in the `squarebrackets.chkdup` option. \cr
#' \cr
#' \cr
#' 
#' @section Sticky:
#' `r .mybadge_option("argument", "sticky")` \cr
#' `r .mybadge_option("option", "sticky")` \cr
#' The \link{slice_x} and \link{slice_rm} methods can already handle names,
#' attributes specific to the \link{mutable_atomic} class,
#' and attributes specific to the \link{factor} class. \cr
#' \cr
#' When `sticky = FALSE`, which is arguably the safest setting,
#' the \link{slice_x} and \link{slice_rm} methods
#' will drop all \bold{other} attributes. \cr
#' \cr
#' By setting `sticky = TRUE`,
#' all attributes except `comment` and `tsp`
#' will be preserved; \cr
#' name-related attributes are separate and are handled by the `use.names` argument. \cr
#' The key advantage for this, is that classes that use `static` attributes
#' (i.e. classes that use attributes that do not change when sub-setting),
#' are automatically supported if `sticky = TRUE`,
#' and no separate methods have to written for \link{slice_x} and \link{slice_rm}. \cr
#' Attributes specific to classes like
#' `difftime`, `Date`, `POSIXct`, `roman`, `hexmode`, `octmode`,
#' and more,
#' use static attributes. \cr
#' \cr
#' Instead of setting `sticky = TRUE` or `sticky = FALSE`,
#' one can also specify all classes that use static attributes
#' that you'll be using in the current R session. \cr
#' In fact, when 'squarebrackets' is loaded
#' (\bold{loaded}, attaching is not necessary),
#' the `squarebrackets.sticky` option is set as follows:
#' 
#' ```{r eval = FALSE, echo = TRUE}
#' 
#' squarebrackets.sticky = c(
#'    "difftime", "Date", "POSIXct", "roman", "hexmode", "octmode"
#' )
#' 
#' ```
#' 
#' So in the above default setting,
#' `sticky = TRUE` for \cr
#' "difftime", "Date", "POSIXct", "roman", "hexmode", "octmode". \cr
#' Also in the above default setting,
#' `sticky = FALSE` for other classes. \cr
#' Note, again, that \link{mutable_atomic} and and \link{factor} are already handled by
#' \link{slice_x} and \link{slice_rm},
#' and their handling is \bold{not} affected by the `sticky` argument/option. \cr
#' \cr
#' The reason the \link{slice_x} and \link{slice_rm} need the `sticky` option,
#' is because of the following. \cr
#' Unlike most `sb_`/`sb2_` methods,
#' the \link{slice_x} and \link{slice_rm} methods
#' are not wrappers around the `[` and `[<-` operators. \cr
#' Therefore,
#' most `[` - S3 methods for highly specialized classes are not readily available for 
#' the \link{slice_x} and \link{slice_rm} methods. \cr
#' And therefore, important class-specific attributes are not automatically preserved. \cr
#' The `sticky` option is a convenient way to support a large number of classes,
#' without having to write specific methods for them. \cr
#' \cr
#' For specialized class that use attributes that \bold{do} change when sub-setting,
#' separate \link{slice_x} and \link{slice_rm} methods need to be written. \cr
#' Package authors are welcome to create method dispatches for their own classes for these methods. \cr
#' \cr
#' As a final note, the name "sticky" is inspired by \code{sticky::}\link[sticky]{sticky}. \cr
#' \cr
#' \cr
#' 
#' @rdname aaa06_squarebrackets_options
#' @name aaa06_squarebrackets_options
#' @aliases squarebrackets_options
NULL
