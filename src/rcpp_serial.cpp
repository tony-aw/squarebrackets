
#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_serial)]]
SEXP rcpp_serial(
  SEXP x
) {
  
  double type = TYPEOF(x);
  
  double maximum = pow(2, 31) - 1;
  double len = Rf_xlength(x);
  double oversized = len - maximum;
  
  double *pres;
  SEXP res = PROTECT(Rf_allocVector(REALSXP, 4));
  pres = REAL(res);
  
  pres[0] = type;
  pres[1] = len;
  pres[2] = oversized;
  pres[3] = 0;
  
  UNPROTECT(1);
  
  return(res);
  
}
 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_is_ma)]]
bool rcpp_is_ma(
  RObject x
) {


  if(!x.hasAttribute("class")) {
    return false;
  }
  String expected_class = "mutable_atomic";
  CharacterVector out_class = x.attr("class");
  bool contains_class = false;
  for(int i = 0; i < out_class.length(); ++i) {
    String current_class = out_class[i];
    if(current_class == expected_class) {
      contains_class = true;
    }
  }
  if(!contains_class) {
    return false;
  }
  
  
  if(!x.hasAttribute("serial")) {
    return false;
  }
  if(TYPEOF(x.attr("serial")) != REALSXP) {
    return false;
  }
  NumericVector out_serial = x.attr("serial");
  NumericVector expected_serial = rcpp_serial(x);
  if(out_serial.length() != expected_serial.length()) {
    return false;
  }
  int n = expected_serial.length();
  for(int i = 0; i < n; ++i) {
    if(out_serial[i] != expected_serial[i]) {
      return false;
    }
  }
  
  
  return true;
  
}
