

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_64)]]
SEXP rcpp_sub2ind_d_64(
  SEXP sub, SEXP dimcumprod
) {



R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

R_xlen_t counter = 0;
R_xlen_t temp = 0;

double *pout;
SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
pout = REAL(out);
MACRO_DIM_DOCALL(MACRO_SUB2IND);
UNPROTECT(1);
return out;

}



