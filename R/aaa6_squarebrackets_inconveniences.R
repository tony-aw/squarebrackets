#' Examples Where the Square Bracket Operators Are Less Convenient
#'
#' @description
#' 
#' This help page shows some examples where the square bracket operators
#' ( `[`, `[<-`)
#' are less than optimally convenient,
#' and how the methods provided by 'squarebrackets' can be helpful
#' in those cases. \cr \cr
#' 
#' 
#' @section Array with Unknown Number of Dimensions:
#' 
#' In order to perform subset operations on some array `x`
#' with the square brackets operator (`[`, `[<-`),
#' one needs to know how many dimensions it has. \cr
#' I.e. if `x` has 3 dimensions, one would use:
#' 
#' ```{r, eval = FALSE, echo = TRUE}
#' 
#' x[i, j, k, drop = FALSE]
#' 
#' x[i, j, k] <- value
#' 
#' ``` 
#' But how would one the use the `[` and `[<-` operators,
#' when number of dimensions of `x` is not known a-priori? \cr
#' It’s not impossible, but still rather convoluted. \cr
#' \cr
#' The methods provided by 'squarebrackets' do not use position-based arguments,
#' and as such work on any arbitrary dimensions without requiring prior knowledge; \cr
#' see \link{squarebrackets_indx_args} for details. \cr \cr
#' 
#' 
#' @section Rule-sets for data.frame-like Objects:
#' 
#' The \link{data.frame}, \link[tibble]{tibble}, \link[data.table]{data.table},
#' and \link[tidytable]{tidytable} classes
#' all inherit from class “data.frame”. \cr
#' Yet they use different rules regarding the usage of the square bracket operators. \cr
#' Constantly switching between these rules is annoying,
#' and makes one's code inconsistent. \cr
#' \cr
#' The methods provided by 'squarebrackets'
#' use the same sub-setting rules for all data.frame inherited classes,
#' thus solving this issue. \cr
#' \cr
#' The 'squarebrackets' package attempts to keep the
#' data.frame methods as class agnostic as possible,
#' through the class agnostic functionality of the 'collapse' and 'data.table' R-packages. \cr
#' This attempt to keep data.frame-like classes consistent does,
#' admittedly,
#' result in some oddities in how data.frames are treated by 'squarebrackets',
#' compared to how other classes are treated by 'squarebrackets':
#' 
#'  * Whole-columns will be auto-coerced when replaced/transformed by \link{sb_mod},
#'  but partial columns will not be auto-coerced by default.
#'  * The \link{sb_x} and \link{sb_rm} methods always automatically conserve all attributes
#'  (though names and dimensions are adjusted accordingly, of course); \cr
#'  the attributes are not stripped, unlike the other classes.
#'  * Giving a data.frame-like object with non-unique column names to the `sb_`-methods
#'  returns an error; \cr
#'  duplicating columns with \link{sb_x}
#'  will automatically adjust the column names to make them unique. \cr \cr
#' 
#' 
#' @section Annoying Sub-setting By Names:
#' 
#' When selecting names for sub-setting,
#' only the first occurrences of the names are selected for the sub-set; \cr
#' and when un-selecting/removing names for sub-setting,
#' the syntax is very different from selecting names. \cr
#' \cr
#' The methods provided by 'squarebrackets'
#' uses the same syntax for both selecting and removing sub-sets. \cr
#' Moreover, selecting/removing sub-sets by names
#' always selects/removes all sub-sets with the given names,
#' not just the first match. \cr \cr
#' 
#' 
#' @section Modification Semantics:
#' 
#' ‘R’ adheres to copy-on-modify semantics when replacing values using \code{[<-}. \cr
#' But sometimes one would like explicit control when to create a copy,
#' and when to modify using pass-by-reference semantics. \cr
#' \cr
#' The 'squarebrackets' package
#' provides the \link{sb_mod} method
#' to return a copy of an object with modified subsets,
#' and the  \link{sb_set} method
#' to modify using pass-by-reference semantics. \cr
#' The \link{idx} method can be used in combination with R's own `[<-` operator
#' for R's default copy-on-modify semantics. \cr \cr
#' 
#' 
#' @section Regarding Other Packages:
#' There are some packages that solve some of these issues. \cr
#' But using different packages for solving different issues for the same common theme
#' (in this case: solving some inconveniences in the square bracket operators)
#' leads to inconsistent code. \cr
#' I have not found an R-package that provides a holistic approach
#' to providing alternative methods to the square brackets operators. \cr
#' Thus, this 'R' package was born. \cr \cr
#' 
#' 
#' 

#' @rdname aaa6_squarebrackets_inconveniences
#' @name aaa6_squarebrackets_inconveniences
#' @aliases squarebrackets_inconveniences
NULL
