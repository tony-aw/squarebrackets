#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_indx_convert_cplx_multi)]]
IntegerVector rcpp_indx_convert_cplx_multi(
  IntegerVector re, IntegerVector im, IntegerVector lens
) {
  int n = lens.length();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i) {
    if(im[i] < 0) {
      out[i] = lens[i] - re[i] + 1;
    }
    else {
      out[i] = re[i];
    }
  }
  return out;
}