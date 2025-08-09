
library(stringi)

SXP_TYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "RAWSXP", "STRSXP")
scalar_types <- c("int", "int", "double", "Rcomplex", "Rbyte", "SEXP")
FUN_TYPES <- c("LOGICAL", "INTEGER", "REAL", "COMPLEX", "RAW", "STRING_PTR")
RCPP_TYPES <- c("Logical", "Integer", "Numeric", "Complex", "Raw", "Character")
SET_FUNS <- c(rep("MACRO_SET_ATOMIC(p", 5), "SET_STRING_ELT(")
COMMENTS <- c(rep("", 5L), "//")

convert_macro <- function(x) {
  x <- stri_split(x, regex = "\\n")[[1L]]
  ind <- 2:(length(x) - 2L)
  x[ind] <- stri_c(x[ind], "\t\\")
  x <- stri_c(x, collapse = "\n")
  return(x)
}




make_atomic_switches <- function(dispatch_variable, keyword_return, template_name, template_inputs, SXP_TYPES, RCPP_TYPES) {
  
  atomic_switches <- "
    switch(TYPEOF(<dispatch_variable>)){
    
      case LGLSXP:
      {
        <return> <fun>_Logical(<inputs>);
        break;
      }
      case INTSXP:
      {
        <return> <fun>_Integer(<inputs>);
        break;
      }
      case REALSXP:
      {
        <return> <fun>_Numeric(<inputs>);
        break;
      }
      case CPLXSXP:
      {
        <return> <fun>_Complex(<inputs>);
        break;
      }
      case RAWSXP:
      {
        <return> <fun>_Raw(<inputs>);
        break;
      }
      case STRSXP:
      {
        <return> <fun>_Character(<inputs>);
        break;
      }
      default: stop(\"unsupported type given\");
    }
    
  "
  
  out <- stri_replace_all_fixed(
    atomic_switches, c("<dispatch_variable>", "<return>", "<fun>", "<inputs>"),
    c(dispatch_variable, keyword_return, template_name, template_inputs), vectorize_all = FALSE
  )
  
  return(out)
  
}

