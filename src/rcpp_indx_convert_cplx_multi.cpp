#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_indx_convert_cplx_multi)]]
NumericVector rcpp_indx_convert_cplx_multi(
  const NumericVector re, const NumericVector im, const NumericVector lens
) {
  int n = lens.length();
  if(re.length() != n || im.length() != n) {
    stop("re, im, and lens must be of equal length");
  }
  NumericVector out(n);
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