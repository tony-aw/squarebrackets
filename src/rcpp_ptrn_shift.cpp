
#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_ptrn_shift)]]
SEXP rcpp_ptrn_shift(SEXP ptrn, R_xlen_t start, R_xlen_t end) {
  R_xlen_t n = Rf_xlength(ptrn);
  R_xlen_t L = std::abs(start - end) + 1;
  R_xlen_t offset = L % n;
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *pout = LOGICAL(out);
  
  const int *pptrn = LOGICAL_RO(ptrn);
  
  for (R_xlen_t i = 0; i < n; ++i) {
    // according to circular indexing theory:
    // j = ((L - 1) - i) % n
    
    // according to modulo algebra:
    // (A - B) % = ((A % n) - B) % n
    
    // therefore, we can rewrite j = ((L - 1) - i)  as:
    // j = ((offset - 1) - i) 
    // where offset = L % n

    R_xlen_t j = (offset - 1 - i) % n;
    if (j < 0) j += n; 
    
    pout[i] = pptrn[j];
    
    // yeah, circular indexing is a nightmare
    
  }
  
  UNPROTECT(1);
  return out;
}
