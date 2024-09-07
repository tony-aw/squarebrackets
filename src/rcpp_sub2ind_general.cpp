#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_setrange)]]
void C_sub2ind_setrange(
  SEXP out, const R_xlen_t start, const R_xlen_t end, const R_xlen_t each, const double myprod, const SEXP rp
) {
  
  R_xlen_t counter_each = 0;
  R_xlen_t counter_rp = 0;
  double temp;
  
  double *pout;
  pout = REAL(out);
  
  switch(TYPEOF(rp)) {
    case INTSXP:
      {
        int *prp;
        prp = INTEGER(rp);
        
        for(R_xlen_t i = start; i <= end; ++i) {
          temp = pout[i] + (prp[counter_rp] - 1) * myprod;
          pout[i] = temp;
          counter_each++;
          if(counter_each == each) {
            counter_rp++;
            counter_each = 0;
          }
        }
        break;
      }
    case REALSXP:
      {
        double *prp;
        prp = REAL(rp);
        
        for(R_xlen_t i = start; i <= end; ++i) {
          temp = pout[i] + (prp[counter_rp] - 1) * myprod;
          pout[i] = temp;
          counter_each++;
          if(counter_each == each) {
            counter_rp++;
            counter_each = 0;
          }
        }
        break;
      }
    default:
      {
        stop("unsupported type");
      }
  }
  
  
}
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_general)]]

NumericVector rcpp_sub2ind_general(
  const List lst, const R_xlen_t total, const NumericVector reps_each, const NumericVector reps_whole, const IntegerVector xdim, const NumericVector dimcumprod
) {
  int ndim = lst.length();
  Rcpp::NumericVector out(total);
  Rcpp::NumericVector coord(total);
  IntegerVector temp1 = lst[0];
  out = rep(temp1, reps_whole[0]);
  if(ndim > 1) {
    for(int j = 1; j < ndim; ++j) {
      double myprod = dimcumprod[j - 1];
      SEXP temp = lst[j];
      R_xlen_t n = Rf_xlength(temp) * reps_each[j];
      for(R_xlen_t i = 0; i < reps_whole[j]; ++i) {
        C_sub2ind_setrange(out, n * i, n * (i + 1) - 1, reps_each[j], myprod, temp);
      }
    }
  }
  return(out);
}
