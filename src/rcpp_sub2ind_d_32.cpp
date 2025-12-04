

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_32)]]
SEXP rcpp_sub2ind_d_32(
  SEXP sub, SEXP dimcumprod
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

R_xlen_t counter = 0;
int temp = 0;

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
pout = INTEGER(out);
MACRO_DIM_DOCALL(MACRO_SUB2IND);
UNPROTECT(1);
return out;

}



