#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_setrange64)]]
void C_sub2ind_setrange64(
  SEXP out, const R_xlen_t start, const R_xlen_t end, const R_xlen_t each, const double myprod, const SEXP rp
) {
  
  R_xlen_t counter_each = 0;
  R_xlen_t counter_rp = 0;
  double temp;
  
  double *pout;
  pout = REAL(out);
  
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
}  
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_general64)]]
NumericVector rcpp_sub2ind_general64(
  const List lst, const R_xlen_t total, const NumericVector reps_each, const NumericVector reps_whole, const IntegerVector xdim, const NumericVector dimcumprod
) {
  int ndim = lst.length();
  Rcpp::NumericVector out(total);
  IntegerVector temp = lst[0];
  out = rep(temp, reps_whole[0]);
  if(ndim > 1) {
    for(int j = 1; j < ndim; ++j) {
      double myprod = dimcumprod[j - 1];
      temp = lst[j];
      R_xlen_t n = Rf_xlength(temp) * reps_each[j];
      for(R_xlen_t i = 0; i < reps_whole[j]; ++i) {
        C_sub2ind_setrange64(out, n * i, n * (i + 1) - 1, reps_each[j], myprod, temp);
      }
    }
  }
  return(out);
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_setrange32)]]
void C_sub2ind_setrange32(
  SEXP out, const R_xlen_t start, const R_xlen_t end, const R_xlen_t each, const double myprod, const SEXP rp
) {
  
  R_xlen_t counter_each = 0;
  R_xlen_t counter_rp = 0;
  double temp;
  
  int *pout;
  pout = INTEGER(out);
  
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
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_general32)]]
IntegerVector rcpp_sub2ind_general32(
  const List lst, const R_xlen_t total, const NumericVector reps_each, const NumericVector reps_whole, const IntegerVector xdim, const IntegerVector dimcumprod
) {
  int ndim = lst.length();
  IntegerVector out(total);
  IntegerVector temp = lst[0];
  out = rep(temp, reps_whole[0]);
  if(ndim > 1) {
    for(int j = 1; j < ndim; ++j) {
      double myprod = dimcumprod[j - 1];
      temp = lst[j];
      R_xlen_t n = Rf_xlength(temp) * reps_each[j];
      for(R_xlen_t i = 0; i < reps_whole[j]; ++i) {
        C_sub2ind_setrange32(out, n * i, n * (i + 1) - 1, reps_each[j], myprod, temp);
      }
    }
  }
  return(out);
}
