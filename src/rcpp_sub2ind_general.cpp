#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_setrange)]]
void rcpp_sub2ind_setrange(
  NumericVector out, const R_xlen_t start, const R_xlen_t end, const R_xlen_t each, const double myprod, const IntegerVector rp
) {
  
  R_xlen_t counter_each = 0;
  R_xlen_t counter_rp = 0;
  double temp;
  for(R_xlen_t i = start; i <= end; ++i) {
    temp = out[i] + (rp[counter_rp] - 1) * myprod;
    out[i] = temp;
    counter_each++;
    if(counter_each == each) {
      counter_rp++;
      counter_each = 0;
    }
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_general)]]
NumericVector rcpp_sub2ind_general(
  const List lst, const R_xlen_t total, const NumericVector reps_each, const NumericVector reps_whole, const NumericVector xdim, const NumericVector dimcumprod
) {
  int ndim = lst.length();
  Rcpp::NumericVector out(total);
  Rcpp::NumericVector coord(total);
  IntegerVector temp = lst[0];
  out = rep(temp, reps_whole[0]);
  if(ndim > 1) {
    for(int j = 1; j < ndim; ++j) {
      double myprod = dimcumprod[j - 1];
      temp = lst[j];
      R_xlen_t n = temp.length() * reps_each[j];
      for(R_xlen_t i = 0; i < reps_whole[j]; ++i) {
        rcpp_sub2ind_setrange(out, n * i, n * (i + 1) - 1, reps_each[j], myprod, temp);
      }
    }
  }
  return(out);
}

