# set-up ====

source("source.R")

library(stringi)



header_for_source <- "

#include <Rcpp.h>
using namespace Rcpp;


inline int rcpp_count_stringmatches(String y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if(y == pv[i]) {
      count++;
    }
  }
  return count;
}




"


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;



inline int rcpp_count_stringmatches(String y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if(y == pv[i]) {
      count++;
    }
  }
  return count;
}

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)


"



macro_slicev <- readr::read_file("macros_slicev.txt")


################################################################################
# countv & whichv ====


code_countv <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_countv)]]
R_xlen_t rcpp_countv(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(len == 0) {
      return count;
    }
    
    MACRO_SLICEV_DO(count++);
  
    return count;
  }
"

cat(code_countv)

code <- paste0(header_for_source,"\n", macro_slicev, "\n", code_countv)
Rcpp::sourceCpp(code = code)


code_whichv <- 
"
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_whichv_32)]]
IntegerVector rcpp_whichv_32(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
   
    R_xlen_t amount = rcpp_countv(y, v, na, invert, start, end, by, len);
    int *pout;
    SEXP out = PROTECT(Rf_allocVector(INTSXP, amount));
    pout = INTEGER(out);
    
    if(amount == 0) {
      UNPROTECT(1);
      return out;
    }
    
    
    MACRO_SLICEV_DO(pout[count] = i + 1; count++);
  
    UNPROTECT(1);
    return out;
  }
  
"

cat(code_whichv)

code <- paste0(header_for_source, macro_slicev, code_countv, code_whichv, collapse = "\n\n")
cat(code)

Rcpp::sourceCpp(code = code)


################################################################################
# slicev_x ====


templatecode <- "

SEXP rcpp_slicev_x_<Rcpp_Type>(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop(\"`x` and `y` must have equal lengths\");
    }
    <scalar_type> *px = <FUN_TYPE>(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
    SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, size));
    <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(<SET_FUN>out, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  
"
templatecodes <- character(6L)

for(i in 1:6) {
  find <- c("<Rcpp_Type>", "<scalar_type>", "<FUN_TYPE>", "<SXP_TYPE>", "<COMMENT>",  "<SET_FUN>")
  replace <- c(RCPP_TYPES[i], scalar_types[i], FUN_TYPES[i], SXP_TYPES[i], COMMENTS[i], SET_FUNS[i])
  templatecodes[i] <- stri_replace_all(
    templatecode, replace, fixed = find, vectorize_all = FALSE
  )
}

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)


switches <- make_atomic_switches(
  "x", "return", "rcpp_slicev_x", "x, y, v, na, invert, start, end, by, len", SXP_TYPES, RCPP_TYPES
)
cat(switches)


code_slicev_x <- stri_c(
  templatecodes,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_x_atomic)]]
SEXP rcpp_slicev_x_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
) {
",
  switches,
  "
  return R_NilValue;
}
"
)

cat(code_slicev_x)


code <- stri_paste(header_for_source, macro_slicev, code_countv, code_whichv, code_slicev_x)
cat(code)
Rcpp::sourceCpp(code = code) # no errors, good!




################################################################################
# slicev_set ====


templatecode <- 
  "

void rcpp_slicev_set_<Rcpp_Type>(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop(\"`x` and `y` must have equal lengths\");
  }
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  <scalar_type> *prp = <FUN_TYPE>(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(<SET_FUN>x, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(<SET_FUN>x, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  "

cat(templatecode)


templatecodes <- character(6L)

for(i in 1:6) {
  find <- c("<Rcpp_Type>", "<scalar_type>", "<FUN_TYPE>", "<SXP_TYPE>", "<COMMENT>",  "<SET_FUN>")
  replace <- c(RCPP_TYPES[i], scalar_types[i], FUN_TYPES[i], SXP_TYPES[i], COMMENTS[i], SET_FUNS[i])
  templatecodes[i] <- stri_replace_all(
    templatecode, replace, fixed = find, vectorize_all = FALSE
  )
}

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)


switches <- make_atomic_switches(
  "x", "", "rcpp_slicev_set", "x, y, v, na, invert, start, end, by, len, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


code_slicev_set <- stri_c(
  templatecodes,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_set_atomic)]]
void rcpp_slicev_set_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
) {
",
  switches,
  "
}
"
)

cat(code_slicev_set)


code <- stri_paste(header_for_source, macro_slicev, code_countv, code_whichv, code_slicev_set)
cat(code)
Rcpp::sourceCpp(code = code) # no errors, good!



################################################################################
# write script ====



code <- stri_paste(
  header_for_package,
  macros_slicev,
  code_countv,
  code_whichv,
  code_slicev_x,
  code_slicev_set,
  collapse = "\n\n"
  
)
cat(code)


setwd("..")
fileConn <- file("src/dynamic_rcpp_slicev.cpp")
writeLines(code, fileConn)
close(fileConn)



