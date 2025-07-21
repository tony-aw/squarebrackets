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
#' The \eqn{\ast}`_x` methods are the only methods
#' where providing duplicate indices actually make sense. \cr
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
#' The \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' can already handle names
#' (through the `use.names` argument),
#' attributes specific to the \link{mutatomic} class,
#' and attributes specific to the \link{factor} class. \cr
#' \cr
#' Attributes which are not names,
#' and not specific to \link{mutatomic} class,
#' and not specific to the \link{factor} class - 
#' henceforth referred to as "other attributes" -
#' are treated differently. \cr
#' How the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' handle these "other" attributes,
#' is determined by the `sticky` option and argument. \cr
#' \cr
#' When `sticky = FALSE`,
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' will drop all \bold{other} attributes. \cr
#' \cr
#' By setting `sticky = TRUE`,
#' all these \bold{other} attributes, except `comment` and `tsp`,
#' will be preserved; \cr
#' The key advantage for this, is that classes that use `static` attributes
#' (i.e. classes that use attributes that do not change when sub-setting),
#' are automatically supported if `sticky = TRUE`,
#' and no separate methods have to written for
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods. \cr
#' Attributes specific to classes like
#' `difftime`, `Date`, `POSIXct`, `roman`, `hexmode`, `octmode`,
#' and more,
#' use static attributes. \cr
#' \cr
#' Instead of setting `sticky = TRUE` or `sticky = FALSE`,
#' one can also specify all classes that use static attributes
#' that you'll be using in the current R session. \cr
#' In fact, when 'squarebrackets' is \bold{loaded},
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
#' \cr
#' The reason
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' need the `sticky` option,
#' is because of the following. \cr
#' Unlike the `fi_`, `fi2_`, `ss_`, and `ss2_` methods,
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' are not wrappers around the `[` and `[<-` operators. \cr
#' Therefore,
#' most `[` - S3 methods for highly specialized classes are not readily available for 
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods. \cr
#' Which in turn means important class-specific attributes are not automatically preserved. \cr
#' The `sticky` option is a convenient way to support a large number of classes,
#' without having to write specific methods for them. \cr
#' \cr
#' For specialized classes that use attributes that \bold{do} change when sub-setting,
#' separate dispatches for
#' the \link{slice_x}, \link{slice_wo}, and \link{slicev_x} methods
#' need to be written. \cr
#' Package authors are welcome to create method dispatches for their own classes for these methods. \cr
#' \cr
#' \cr
#' 
#' @rdname aaa06_squarebrackets_options
#' @name aaa06_squarebrackets_options
#' @aliases squarebrackets_options
NULL
