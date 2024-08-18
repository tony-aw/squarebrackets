#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_any_badindx)]]
bool rcpp_any_badindx(IntegerVector indx, int value) {
  R_xlen_t n = indx.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(indx[i] > value || indx[i] < 1) return true;
  }
  return false;
}
