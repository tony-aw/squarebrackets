#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_anybad)]]
bool rcpp_anybad(IntegerVector indx, int value) {
  R_xlen_t n = indx.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(indx[i] > value || indx[i] < 1) return true;
  }
  return false;
}
