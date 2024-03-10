#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_mlen)]]
List rcpp_mlen(IntegerVector x) {
  int n = x.length();
  List out(n);
  for(int i = 0; i < n; ++i) {
    IntegerVector temp = seq_len(x[i]);
    out[i] = temp;
  }
  return out;
}


