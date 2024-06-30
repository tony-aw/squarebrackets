#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setvind_Complex)]]
void rcpp_setvind_Complex(ComplexVector x, IntegerVector ind, ComplexVector rp) {
  R_xlen_t n = ind.length();
  R_xlen_t n_rp = rp.length();
  R_xlen_t counter = 0;
  if(n_rp == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(n_rp == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}
