

#include <Rcpp.h>

using namespace Rcpp;







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Logical)]]
void rcpp_set_array_2d_Logical(
  LogicalVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, LogicalVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Logical)]]
void rcpp_set_array_3d_Logical(
  LogicalVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, LogicalVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Logical)]]
void rcpp_set_array_4d_Logical(
  LogicalVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, LogicalVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Logical)]]
void rcpp_set_array_5d_Logical(
  LogicalVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, LogicalVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Logical)]]
void rcpp_set_array_6d_Logical(
  LogicalVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, LogicalVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Integer)]]
void rcpp_set_array_2d_Integer(
  IntegerVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, IntegerVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Integer)]]
void rcpp_set_array_3d_Integer(
  IntegerVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, IntegerVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Integer)]]
void rcpp_set_array_4d_Integer(
  IntegerVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, IntegerVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Integer)]]
void rcpp_set_array_5d_Integer(
  IntegerVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, IntegerVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Integer)]]
void rcpp_set_array_6d_Integer(
  IntegerVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, IntegerVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Numeric)]]
void rcpp_set_array_2d_Numeric(
  NumericVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, NumericVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Numeric)]]
void rcpp_set_array_3d_Numeric(
  NumericVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, NumericVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Numeric)]]
void rcpp_set_array_4d_Numeric(
  NumericVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, NumericVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Numeric)]]
void rcpp_set_array_5d_Numeric(
  NumericVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, NumericVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Numeric)]]
void rcpp_set_array_6d_Numeric(
  NumericVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, NumericVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Character)]]
void rcpp_set_array_2d_Character(
  CharacterVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, CharacterVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Character)]]
void rcpp_set_array_3d_Character(
  CharacterVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, CharacterVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Character)]]
void rcpp_set_array_4d_Character(
  CharacterVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, CharacterVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Character)]]
void rcpp_set_array_5d_Character(
  CharacterVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, CharacterVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Character)]]
void rcpp_set_array_6d_Character(
  CharacterVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, CharacterVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Complex)]]
void rcpp_set_array_2d_Complex(
  ComplexVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, ComplexVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Complex)]]
void rcpp_set_array_3d_Complex(
  ComplexVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, ComplexVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Complex)]]
void rcpp_set_array_4d_Complex(
  ComplexVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, ComplexVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Complex)]]
void rcpp_set_array_5d_Complex(
  ComplexVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, ComplexVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Complex)]]
void rcpp_set_array_6d_Complex(
  ComplexVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, ComplexVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Raw)]]
void rcpp_set_array_2d_Raw(
  RawVector x, IntegerVector ind1, IntegerVector ind2, NumericVector dimcumprod, RawVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
double flatind = 0;
  
if(rp.length() == (ni * nj)) {
R_xlen_t counter = 0;
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Raw)]]
void rcpp_set_array_3d_Raw(
  RawVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, NumericVector dimcumprod, RawVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk)) {
R_xlen_t counter = 0;
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Raw)]]
void rcpp_set_array_4d_Raw(
  RawVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, NumericVector dimcumprod, RawVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl)) {
R_xlen_t counter = 0;
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Raw)]]
void rcpp_set_array_5d_Raw(
  RawVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, NumericVector dimcumprod, RawVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm)) {
R_xlen_t counter = 0;
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Raw)]]
void rcpp_set_array_6d_Raw(
  RawVector x, IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, NumericVector dimcumprod, RawVector rp
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
double flatind = 0;
  
if(rp.length() == (ni * nj * nk * nl * nm * nn)) {
R_xlen_t counter = 0;
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      flatind = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Logical)]]
void rcpp_set_array_2d_6d_Logical(
  LogicalVector x, List out, NumericVector dimcumprod, LogicalVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Logical(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Logical(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Logical(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Logical(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Logical(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Integer)]]
void rcpp_set_array_2d_6d_Integer(
  IntegerVector x, List out, NumericVector dimcumprod, IntegerVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Integer(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Integer(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Integer(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Integer(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Integer(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Numeric)]]
void rcpp_set_array_2d_6d_Numeric(
  NumericVector x, List out, NumericVector dimcumprod, NumericVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Numeric(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Numeric(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Numeric(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Numeric(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Numeric(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Character)]]
void rcpp_set_array_2d_6d_Character(
  CharacterVector x, List out, NumericVector dimcumprod, CharacterVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Character(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Character(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Character(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Character(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Character(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Complex)]]
void rcpp_set_array_2d_6d_Complex(
  ComplexVector x, List out, NumericVector dimcumprod, ComplexVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Complex(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Complex(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Complex(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Complex(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Complex(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Raw)]]
void rcpp_set_array_2d_6d_Raw(
  RawVector x, List out, NumericVector dimcumprod, RawVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Raw(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Raw(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Raw(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Raw(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Raw(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
  }
}


