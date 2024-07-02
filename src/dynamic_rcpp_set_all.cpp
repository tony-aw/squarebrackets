

#include <Rcpp.h>

using namespace Rcpp;









//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Logical)]]
void rcpp_set_all_Logical(LogicalVector x, LogicalVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Integer)]]
void rcpp_set_all_Integer(IntegerVector x, IntegerVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Numeric)]]
void rcpp_set_all_Numeric(NumericVector x, NumericVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Character)]]
void rcpp_set_all_Character(CharacterVector x, CharacterVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Complex)]]
void rcpp_set_all_Complex(ComplexVector x, ComplexVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_Raw)]]
void rcpp_set_all_Raw(RawVector x, RawVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}


