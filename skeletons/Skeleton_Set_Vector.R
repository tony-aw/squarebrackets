
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
# 32 bit ====
#

templatecode32 <- "

inline void rcpp_set_vind_32_<RCPP_TYPE>(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      <SET_FUN>x, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      <SET_FUN>x, pind[i] - 1, prp[0]);
    }
  }
  else stop(\"recycling not allowed\");
  
}

"


templatecodes32 <- character(6L)
for(i in 1:6) {
  
  search <- c("<RCPP_TYPE>", "<SXP_TYPE>", "<scalar_type>", "<FUN_TYPE>", "<SET_FUN>", "<COMMENT>")
  replace <- c(RCPP_TYPES[i], SXP_TYPES[i], scalar_types[i], FUN_TYPES[i], SET_FUNS[i], COMMENTS[i])
  
  templatecodes32[i] <- stri_replace_all(
    templatecode32,
    replace,
    fixed = search,
    vectorize_all = FALSE
  )
}

templatecodes32 <- stri_c(templatecodes32, collapse = "\n\n")

cat(templatecodes32)




switches <- make_atomic_switches(
  "x", "", "rcpp_set_vind_32", "x, ind, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


atomic_code32 <- stri_c("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_atomic)]]
void rcpp_set_vind_32_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {
",
               
switches,
"

}


"
)



################################################################################
# 64 bit ====
#

templatecode64 <- "

inline void rcpp_set_vind_64_<RCPP_TYPE>(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      <SET_FUN>x, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      <SET_FUN>x, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop(\"recycling not allowed\");
  
}

"


templatecodes64 <- character(6L)
for(i in 1:6) {
  
  search <- c("<RCPP_TYPE>", "<SXP_TYPE>", "<scalar_type>", "<FUN_TYPE>", "<SET_FUN>", "<COMMENT>")
  replace <- c(RCPP_TYPES[i], SXP_TYPES[i], scalar_types[i], FUN_TYPES[i], SET_FUNS[i], COMMENTS[i])
  
  templatecodes64[i] <- stri_replace_all(
    templatecode64,
    replace,
    fixed = search,
    vectorize_all = FALSE
  )
}

templatecodes64 <- stri_c(templatecodes64, collapse = "\n\n")

cat(templatecodes64)




switches <- make_atomic_switches(
  "x", "", "rcpp_set_vind_64", "x, ind, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


atomic_code64 <- stri_c("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_atomic)]]
void rcpp_set_vind_64_atomic(
  SEXP x, const SEXP ind, const SEXP rp
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
  c(header_for_source, templatecodes32, atomic_code32, templatecodes64, atomic_code64),
  collapse = "\n\n\n"
)
cat(code)

Rcpp::sourceCpp(code = code) # no errors! Good :-)



################################################################################
# write code ====
#


code <-  paste(
  c(header_for_package, templatecodes32, atomic_code32, templatecodes64, atomic_code64),
  collapse = "\n\n\n"
)
cat(code)

setwd("..")
fileConn <- file("src/dynamic_rcpp_set_vind.cpp")
writeLines(code, fileConn)
close(fileConn)


