#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_prod_int)]]
int rcpp_prod_int(IntegerVector x) {
  R_xlen_t n = x.length();
  int out = x[0];
  if(n > 1) {
    for(R_xlen_t i = 1; i < n; ++i) {
      out = out * x[i];
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_pre_coord2ind)]]
void rcpp_pre_coord2ind(IntegerVector ind, IntegerVector coord, int myprod) {
  R_xlen_t n = ind.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    ind[i] = ind[i] + myprod * (coord[i] - 1);
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_coord2ind)]]
IntegerVector rcpp_coord2ind(
  IntegerVector ind, IntegerMatrix coord, IntegerVector xdim
) {
  int nc = coord.ncol();
  if(nc > 1) {
    IntegerVector temp;
    int myprod;
    for(R_xlen_t i = 1; i < nc; ++i) {
      myprod = rcpp_prod_int(xdim[Range(0, i-1)]);
      temp = coord(_, i);
      rcpp_pre_coord2ind(ind, temp, myprod);
    }
  }
  
  return ind;
}
