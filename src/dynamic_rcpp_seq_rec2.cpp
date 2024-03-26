

#include <Rcpp.h>

using namespace Rcpp;






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_plus)]]
NumericVector rcpp_seq_rec2_plus(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop("`inits`, `s`, `m` must each be of length 2");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[idx] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) + (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) + (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_min)]]
NumericVector rcpp_seq_rec2_min(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop("`inits`, `s`, `m` must each be of length 2");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[idx] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) - (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) - (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_x)]]
NumericVector rcpp_seq_rec2_x(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop("`inits`, `s`, `m` must each be of length 2");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[idx] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) * (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) * (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_div)]]
NumericVector rcpp_seq_rec2_div(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop("`inits`, `s`, `m` must each be of length 2");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[idx] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) / (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) / (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}


