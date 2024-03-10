#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_general)]]
IntegerVector rcpp_sub2ind_general(
  List lst, int total, IntegerVector reps_each, IntegerVector reps_whole, IntegerVector xdim, IntegerVector dimcumprod
) {
  int ndim = lst.length();
  Rcpp::IntegerVector out(total);
  Rcpp::IntegerVector coord(total);
  IntegerVector temp = lst[0];
  out = rep(temp, reps_whole[0]);
  if(ndim > 1) {
    for(int j = 1; j < ndim; ++j) {
      int myprod = dimcumprod[j - 1];
      temp = lst[j];
      R_xlen_t n = temp.length() * reps_each[j];
      for(int i = 0; i < reps_whole[j]; ++i) {
        Range myrng = Range(n * i, n * (i + 1) - 1);
        coord[myrng] = rep_each(temp, reps_each[j]);
        out[myrng] = out[myrng] + (coord[myrng] - 1) * myprod;
      }
    }
  }
  return(out);
}
