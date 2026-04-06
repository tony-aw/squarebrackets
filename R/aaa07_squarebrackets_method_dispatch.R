#' Ellipses Usage and Method Dispatches in 'squarebrackets'
#'
#' @description
#' 
#' This help page gives some additional details regarding the S3 method dispatch
#' used in 'squarebrackets'. \cr \cr
#' 
#' 
#' 
#' @section Ellipsis:
#' Due to how the S3 method dispatch system works in 'R',
#' all generic methods have the ellipsis argument (`...`). \cr
#' For the user's safety,
#' 'squarebrackets' does check that the user doesn't accidentally
#' add arguments that make no sense for that method. \cr
#' \cr
#' \cr
#' 
#' @section 'squarebrackets' Methods that do not require explicit Dispatches: 
#' The `ii_`/`ss_`/`tt_` - `_x` methods
#' are essentially wrappers around
#' the `[` operator. \cr
#' Similarly,
#' the `ii_`/`ss_`/`tt_` - `_mod` methods
#' are essentially wrappers around
#' the `[<-` operator. \cr
#' And the \link{lst_rec} and \link{lst_recin} methods
#' are essentially wrappers around
#' the `[[` and `[[<-` operators. \cr
#' Therefore, any custom class that has method dispatches defined for
#' the `[`, `[<-`, `[[`, and `[[<-` methods,
#' have their dispatches automatically handled by the above named methods. \cr
#' \cr
#' 
#' @section 'squarebrackets' Methods that DO require explicit Dispatches:
#' Unlike the `ii_`/`ss_`/`tt_` - `_x` methods,
#' the \link{long_x} method is \bold{not} a wrapper around the `[` operator,
#' and thus does not detect custom method dispatches defined for `[`. \cr
#' \link{long_x} can support classes that have static attributes
#' (attributes that do not depend on the type, length, or data of the vector),
#' by adding the class names to 
#' the \link[=squarebrackets_options]{squarebrackets.sticky} option. \cr
#' Classes with non-static attributes will explicitly require a `long_x` method. \cr
#' \cr
#' 
#' @section Class support for the Pass-By-Reference Methods:
#' The `_set` methods only support the `data.table` and `mutatomic` classes. \cr
#' Other mutable classes are not supported by the 'squarebrackets' package. \cr
#' However,
#' other 'R' package authors are welcome to add additional method dispatches
#' for the `_set` methods. \cr
#' \cr
#' 
#' 
#' 
#' 

#' @rdname aaa07_squarebrackets_method_dispatch
#' @name aaa07_squarebrackets_method_dispatch
#' @aliases squarebrackets_method_dispatch
NULL
