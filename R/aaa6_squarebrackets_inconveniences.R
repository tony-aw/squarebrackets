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
#' The methods provided by 'squarebrackets' solve this by using
#' name-based arguments, instead of position based arguments. \cr \cr
#' 
#' 
#' @section Different Rulesets for data.frame-like Objects:
#' 
#' he \link{data.frame}, \link[tibble]{tibble}, \link[data.table]{data.table},
#' and \link[tidytable]{tidytable} classes
#' all inherit from class “data.frame”. \cr
#' Yet they use different rules regarding the usage of the square bracket operators. \cr
#' Constantly switching between these rules is annoying,
#' and makes one's code inconsistent. \cr
#' \cr
#' The methods provided by 'squarebrackets'
#' use the same sub-setting rules for all data.frame inherited classes,
#' thus solving this issue. \cr \cr
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
#' @section Copy Semantics:
#' 
#' ‘R’ adheres to copy-on-modification semantics when replacing values using \code{[<-}. \cr
#' But sometimes one would like explicit control when to create a copy,
#' and when to modify using pass-by-reference semantics. \cr
#' \cr
#' The 'squarebrackets' package
#' provides the \link{sb_mod} method
#' to return a copy of an object with modified subsets,
#' and the  \link{sb_set} method
#' to modify using pass-by-reference semantics. \cr \cr
#' 
#' 
#' @section Regarding Other Packages:
#' There are some packages that solve some of these issues. \cr
#' But using different packages for solving different issues for the same common theme
#' (in this case: solving some inconveniences in the square bracket operators)
#' leads to inconsistent code. \cr
#' I have not found an R-package that provides a holistic approach
#' to providing alternative methods to the square brackets operators. \cr
#' Thus this 'R' package was born. \cr \cr
#' 
#' 
#' 

#' @rdname aaa6_squarebrackets_inconveniences
#' @name aaa6_squarebrackets_inconveniences
#' @aliases squarebrackets_inconveniences
NULL
