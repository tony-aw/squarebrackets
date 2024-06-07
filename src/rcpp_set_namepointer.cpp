#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_namepointer)]]
void rcpp_set_namepointer(
  CharacterVector nms, IntegerVector ind, CharacterVector rp
) {
  int n = rp.length();
  if(n != ind.length()) {
    stop("ind.length() not equal to rp.length()");
  }
  for(R_xlen_t i = 0; i < n; ++i) {
    nms[ind[i]] = rp[i];
  }
}