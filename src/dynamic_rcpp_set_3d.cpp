

#include <Rcpp.h>

using namespace Rcpp;







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Logical)]]
void rcpp_set_3d_Logical(
  LogicalVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  LogicalVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Integer)]]
void rcpp_set_3d_Integer(
  IntegerVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  IntegerVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Numeric)]]
void rcpp_set_3d_Numeric(
  NumericVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  NumericVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Character)]]
void rcpp_set_3d_Character(
  CharacterVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  CharacterVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Complex)]]
void rcpp_set_3d_Complex(
  ComplexVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  ComplexVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_Raw)]]
void rcpp_set_3d_Raw(
  RawVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  RawVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}


