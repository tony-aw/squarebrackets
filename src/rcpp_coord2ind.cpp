#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_prod_int)]]
int C_prod_int(SEXP x, int start, int end) {
  R_xlen_t n = end - start + 1;
  
  int *px;
  px = INTEGER(x);
  
  int out = px[0];
  
  if(n > 1) {
    for(R_xlen_t i = start; i < end; ++i) {
      out = out * px[i];
    }
  }
  return(out);
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_pre_coord2ind)]]
void C_pre_coord2ind(SEXP ind, SEXP coord, int myprod) {
  R_xlen_t n = Rf_xlength(ind);
  
  double *pind;
  pind = REAL(ind);
  
  int *pcoord;
  pcoord = INTEGER(coord);
  
  for(R_xlen_t i = 0; i < n; ++i) {
    pind[i] = pind[i] + myprod * (pcoord[i] - 1);
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_coord2ind)]]
NumericVector rcpp_coord2ind(
  NumericVector ind, IntegerMatrix coord, IntegerVector xdim
) {
  R_xlen_t nc = coord.ncol();
  if(nc > 1) {
    IntegerVector temp;
    int myprod;
    for(R_xlen_t i = 1; i < nc; ++i) {
      myprod = C_prod_int(xdim, 0, i-1);
      temp = coord(_, i);
      C_pre_coord2ind(ind, temp, myprod);
    }
  }
  
  return ind;
}