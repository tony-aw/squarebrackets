#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec)]]
NumericVector rcpp_seq_rec(NumericVector inits, int w, int n, Function f) {
  NumericVector x(n);
  Range idx(0, w - 1);
  x[idx] = inits;
  for (int i = w; i < n; i++){
    Range idx(i - w, i - 1);
    x[i] = as<double>(f(x[idx]));
  }
  return x;
}


