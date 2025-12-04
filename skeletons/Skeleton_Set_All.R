
library(stringi)
source("source.R")


header_for_source <- "

#include <Rcpp.h>

using namespace Rcpp;

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

"


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;


"



################################################################################
# template code ====
#

templatecode <- "

inline void rcpp_set_all_<RCPP_TYPE>(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      <SET_FUN>x, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      <SET_FUN>x, i, prp[0]);
    }
  }
  else stop(\"recycling not allowed\");
  
}

"


templatecodes <- character(6L)
for(i in 1:6) {
  
  search <- c("<RCPP_TYPE>", "<SXP_TYPE>", "<scalar_type>", "<FUN_TYPE>", "<SET_FUN>", "<COMMENT>")
  replace <- c(RCPP_TYPES[i], SXP_TYPES[i], scalar_types[i], FUN_TYPES[i], SET_FUNS[i], COMMENTS[i])
  
  templatecodes[i] <- stri_replace_all(
    templatecode,
    replace,
    fixed = search,
    vectorize_all = FALSE
  )
}

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)




switches <- make_atomic_switches(
  "x", "", "rcpp_set_all", "x, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


atomic_code <- stri_c("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_atomic)]]
void rcpp_set_all_atomic(
  SEXP x, const SEXP rp
) {
",
               
switches,
"

}


"
)




################################################################################
# Checkcode ====
#


code <-  paste(
  c(header_for_source, templatecodes, atomic_code),
  collapse = "\n\n\n"
)
cat(code)

Rcpp::sourceCpp(code = code) # no errors! Good :-)



################################################################################
# write code ====
#


code <-  paste(
  c(header_for_package, templatecodes, atomic_code),
  collapse = "\n\n\n"
)
cat(code)

setwd("..")
fileConn <- file("src/dynamic_rcpp_set_all.cpp")
writeLines(code, fileConn)
close(fileConn)


